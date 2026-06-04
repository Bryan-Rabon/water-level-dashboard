library(DBI)
library(RSQLite)
library(dplyr)
library(jsonlite)
library(lubridate)

# ── CONFIG ────────────────────────────────────────────────────────────────────
DB_FILE_PATH <- "scor_site_data.sqlite"
OUTPUT_DIR   <- "analysis"
GIT_MSG      <- paste0("Auto-update analysis JSON [", format(Sys.time(), "%Y-%m-%d %H:%M"), "]")

# ── CONNECT ───────────────────────────────────────────────────────────────────
con <- dbConnect(RSQLite::SQLite(), DB_FILE_PATH)
message("Database connection established.")

# ── 1. SITE SUMMARY (existing + enriched) ─────────────────────────────────────
# Added: first_sample, stddev, recent_avg (last 7 days), recent_count
message("Running site_summary query...")

summary_results <- dbGetQuery(con, "
  SELECT
    SITE,
    Parameter,
    AVG(Result)                             AS avg_result,
    MIN(Result)                             AS min_result,
    MAX(Result)                             AS max_result,
    COUNT(*)                                AS reading_count,
    MIN(Units)                              AS unit,
    MIN(DATETIME)                           AS first_sample,
    MAX(DATETIME)                           AS last_sample,
    -- Variance computed inline; take sqrt in R for stddev
    AVG(Result * Result) - AVG(Result) * AVG(Result) AS variance
  FROM scor_data
  GROUP BY SITE, Parameter
")

# Recent stats: last 7 days per site/parameter
recent_cutoff <- format(Sys.time() - days(7), "%Y-%m-%dT%H:%M:%SZ")

recent_results <- dbGetQuery(con, paste0("
  SELECT
    SITE,
    Parameter,
    AVG(Result)  AS recent_avg,
    COUNT(*)     AS recent_count
  FROM scor_data
  WHERE DATETIME >= '", recent_cutoff, "'
  GROUP BY SITE, Parameter
"))

dbDisconnect(con)
message("Database connection closed.")

# ── FORMAT SITE SUMMARY ───────────────────────────────────────────────────────
site_summary <- summary_results %>%
  left_join(recent_results, by = c("SITE", "Parameter")) %>%
  mutate(
    # Stddev from variance
    stddev          = sqrt(pmax(variance, 0)),
    # Days active
    first_sample    = as.POSIXct(first_sample, tz = "UTC"),
    last_sample_dt  = as.POSIXct(last_sample,  tz = "UTC"),
    days_active     = as.numeric(difftime(last_sample_dt, first_sample, units = "days")),
    # Trend: recent avg vs overall avg (positive = recently higher than normal)
    trend_delta     = round(recent_avg - avg_result, 4),
    # Format dates for JSON
    first_sample    = format(first_sample,   "%Y-%m-%d %H:%M"),
    last_sample     = format(last_sample_dt, "%Y-%m-%d %H:%M"),
    # Round numerics
    avg_result      = round(avg_result,  4),
    min_result      = round(min_result,  4),
    max_result      = round(max_result,  4),
    stddev          = round(stddev,       4),
    recent_avg      = round(recent_avg,   4),
    days_active     = round(days_active,  1)
  ) %>%
  select(SITE, Parameter, unit, avg_result, min_result, max_result, stddev,
         recent_avg, recent_count, trend_delta,
         reading_count, days_active, first_sample, last_sample)

# ── 2. NETWORK HEALTH SUMMARY ─────────────────────────────────────────────────
# One row per site — useful for the analysis page header counts and a
# potential network-health widget showing overall fleet status
message("Building network_health summary...")

seven_days_ago <- Sys.time() - days(7)

network_health <- site_summary %>%
  group_by(SITE) %>%
  summarize(
    last_sample      = max(last_sample),
    total_readings   = sum(reading_count),
    recent_readings  = sum(recent_count),
    parameters       = n(),
    days_active      = max(days_active),
    .groups = "drop"
  ) %>%
  mutate(
    last_sample_dt = as.POSIXct(last_sample, format = "%Y-%m-%d %H:%M", tz = "UTC"),
    status = case_when(
      last_sample_dt >= seven_days_ago ~ "recent",
      !is.na(last_sample_dt)           ~ "stale",
      TRUE                              ~ "none"
    ),
    site_short = substr(SITE, 1, 8)
  ) %>%
  select(site_short, SITE, status, last_sample, total_readings,
         recent_readings, parameters, days_active)

# ── 3. PARAMETER NETWORK STATS ────────────────────────────────────────────────
# Cross-site stats per parameter — useful for comparing a site vs the network
# e.g. "this site's depth avg vs all sites' depth avg"
message("Building parameter_stats summary...")

parameter_stats <- site_summary %>%
  group_by(Parameter, unit) %>%
  summarize(
    network_avg    = round(mean(avg_result,  na.rm = TRUE), 4),
    network_min    = round(min(min_result,   na.rm = TRUE), 4),
    network_max    = round(max(max_result,   na.rm = TRUE), 4),
    network_stddev = round(sd(avg_result,    na.rm = TRUE), 4),
    site_count     = n(),
    .groups = "drop"
  )

# ── EXPORT ALL JSON FILES ─────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

write_json(site_summary,    file.path(OUTPUT_DIR, "site_summary.json"),    pretty = TRUE)
write_json(network_health,  file.path(OUTPUT_DIR, "network_health.json"),  pretty = TRUE)
write_json(parameter_stats, file.path(OUTPUT_DIR, "parameter_stats.json"), pretty = TRUE)

message("JSON files written:")
message("  - analysis/site_summary.json    (", nrow(site_summary),    " rows)")
message("  - analysis/network_health.json  (", nrow(network_health),  " rows)")
message("  - analysis/parameter_stats.json (", nrow(parameter_stats), " rows)")

# ── GIT AUTO-COMMIT & PUSH ────────────────────────────────────────────────────
message("Committing and pushing to GitHub...")

run_git <- function(args) {
  result <- system2("git", args, stdout = TRUE, stderr = TRUE)
  status <- attr(result, "status")
  if (!is.null(status) && status != 0) {
    warning(paste("Git command failed:", paste(args, collapse = " "),
                  "\n", paste(result, collapse = "\n")))
  } else {
    message(paste("git", paste(args, collapse = " ")))
  }
  invisible(result)
}
# 
# # Stage all three files
# run_git(c("add", file.path(OUTPUT_DIR, "site_summary.json")))
# run_git(c("add", file.path(OUTPUT_DIR, "network_health.json")))
# run_git(c("add", file.path(OUTPUT_DIR, "parameter_stats.json")))
# run_git(c("commit", "-m", GIT_MSG))
# run_git(c("push"))
# 
# message("--- Done. All analysis files committed and pushed to GitHub. ---")