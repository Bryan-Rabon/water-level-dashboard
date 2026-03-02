library(DBI)
library(RSQLite)
library(dplyr)
library(jsonlite)
library(lubridate) # Helpful for date handling

# 1. Connect
con <- dbConnect(RSQLite::SQLite(), "scor_site_data.sqlite")

# 2. Read Tables
scor_data <- dbReadTable(con, "scor_data")

# 3. Process with Date logic
summary_results <- scor_data %>%
  mutate(SITE_SHORT = substr(SITE, 1, 8)) %>%
  # Convert string to Date-Time object so max() works correctly
  mutate(DATETIME = as.POSIXct(DATETIME)) %>% 
  group_by(SITE, Parameter) %>%
  summarize(
    avg_result = mean(Result, na.rm = TRUE),
    min_result = min(Result, na.rm = TRUE),
    max_result = max(Result, na.rm = TRUE),
    reading_count = n(),
    unit = first(Units),
    # Get the most recent date and format it back to a readable string
    last_sample = max(DATETIME, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  # Optional: Format the date for the web (e.g., "2024-05-20")
  mutate(last_sample = format(last_sample, "%Y-%m-%d %H:%M"))

# 4. Export
if (!dir.exists("analysis")) dir.create("analysis")
write_json(summary_results, "analysis/site_summary.json", pretty = TRUE)

dbDisconnect(con)