library(DBI)
library(RSQLite)
library(dplyr)
library(jsonlite)
library(lubridate)

# ── CONFIG ────────────────────────────────────────────────────────────────────
DB_FILE_PATH <- "scor_site_data.sqlite"
OUTPUT_DIR   <- "analysis"
OUTPUT_FILE  <- file.path(OUTPUT_DIR, "site_summary.json")
GIT_MSG      <- paste0("Auto-update site_summary.json [", format(Sys.time(), "%Y-%m-%d %H:%M"), "]")

# ── CONNECT ───────────────────────────────────────────────────────────────────
con <- dbConnect(RSQLite::SQLite(), DB_FILE_PATH)
message("Database connection established.")

# ── SUMMARISE DIRECTLY IN SQL ─────────────────────────────────────────────────
# Aggregation happens inside SQLite — only the summary rows are returned to R,
# not the full dataset. This stays fast regardless of how large the DB grows.
message("Running summary query in SQLite...")

summary_results <- dbGetQuery(con, "
  SELECT
    SITE,
    Parameter,
    AVG(Result)                         AS avg_result,
    MIN(Result)                         AS min_result,
    MAX(Result)                         AS max_result,
    COUNT(*)                            AS reading_count,
    MIN(Units)                          AS unit,
    MAX(DATETIME)                       AS last_sample
  FROM scor_data
  GROUP BY SITE, Parameter
")

message(paste("Summary complete:", nrow(summary_results), "rows across",
              n_distinct(summary_results$SITE), "sites."))

# ── DISCONNECT EARLY ──────────────────────────────────────────────────────────
# No need to hold the connection open during file writing or git operations
dbDisconnect(con)
message("Database connection closed.")

# ── FORMAT & EXPORT ───────────────────────────────────────────────────────────
# Format last_sample to readable string after pulling from DB
summary_results <- summary_results %>%
  mutate(last_sample = format(as.POSIXct(last_sample, tz = "UTC"), "%Y-%m-%d %H:%M"))

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)
write_json(summary_results, OUTPUT_FILE, pretty = TRUE)
message(paste("JSON written to:", OUTPUT_FILE))

# ── GIT AUTO-COMMIT & PUSH ────────────────────────────────────────────────────
message("Committing and pushing to GitHub...")

run_git <- function(args) {
  result <- system2("git", args, stdout = TRUE, stderr = TRUE)
  status <- attr(result, "status")
  if (!is.null(status) && status != 0) {
    warning(paste("Git command failed:", paste(args, collapse = " "),
                  "\n", paste(result, collapse = "\n")))
  } else {
    message(paste("git", paste(args, collapse = " "), "->",
                  paste(result, collapse = " ")))
  }
  invisible(result)
}

run_git(c("add", OUTPUT_FILE))
run_git(c("commit", "-m", GIT_MSG))
run_git(c("push"))

message("--- Done. site_summary.json committed and pushed to GitHub. ---")