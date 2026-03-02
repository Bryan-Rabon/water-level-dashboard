library(tidyverse)
library(sf)
library(openxlsx)
library(DBI)
library(RSQLite)
library(dbplyr)


source("functions.R")

# Define the database file and table name
DB_FILE_PATH <- "scor_site_data.sqlite"
TABLE_NAME <- "scor_data"
SITE_TABLE_NAME <- "Sites"

# Connect to (or create) the SQLite database file
con <- DBI::dbConnect(RSQLite::SQLite(), DB_FILE_PATH)
message(paste("Database connection established for:", DB_FILE_PATH))

# Explicitly create the table if it doesn't exist
if (!DBI::dbExistsTable(con, TABLE_NAME)) {
  # We define the columns expected from the API response
  create_table_sql <- paste0("
    CREATE TABLE ", TABLE_NAME, " (
      SITE TEXT,
      DATETIME TEXT,
      Value REAL,
      -- Add more columns here if your API response includes them
      PRIMARY KEY (SITE, DATETIME) 
    );
  ")
  
  DBI::dbExecute(con, create_table_sql)
  message(paste("✅ Created new table:", TABLE_NAME))
}


# Explicitly construct the URL to query the layer and request GeoJSON output
feature_query_url <- "https://services3.arcgis.com/iQDjAnwEnrAur0g1/arcgis/rest/services/scor_sites_Deployed/FeatureServer/0/query?where=1=1&outFields=*&f=geojson"

print("Attempting to read data using explicit GeoJSON query...")
scor_sites_sf <- st_read(feature_query_url)

st_write(scor_sites_sf, con, "Sites", delete_layer = TRUE)
nc2 <- st_read(con, "Sites")

sites <- scor_sites_sf |>
  st_drop_geometry() |> 
  distinct(SITENAME) |> 
  pull(SITENAME)

START_TIME <- "2025-01-01 00:00:00"
END_TIME <- Sys.time() |>
  ceiling_date(unit = "day") |>
  format("%Y-%m-%d %H:%M:%S")



#get_scor_data_df (sites[1],START_TIME , END_TIME)

all_scor_data <- purrr::map_dfr(
  .x = sites, 
  .f = get_scor_data_df,
  start_date = START_TIME,
  end_date = END_TIME,
  .progress = TRUE # Shows a progress bar
)

# Reference the table with dbplyr
data_tbl <- tbl(con, TABLE_NAME)

data_tbl %>%
  rows_upsert(all_scor_data, by = c("SITE","DATETIME","Parameter","Units"), copy = TRUE, in_place = TRUE)


#Uncomment to find stations that do not download
# notrunerror <- sites[!(sites %in% (all_scor_data$SITE |> unique()))]
# 
# all_scor_data2 <- purrr::map_dfr(
#   .x = notrunerror, 
#   .f = get_scor_data_df,
#   start_date = START_TIME,
#   end_date = END_TIME,
#   .progress = TRUE # Shows a progress bar
# )


# Always disconnect from the database when finished
DBI::dbDisconnect(con)
message("--- Database connection closed. Task complete. ---")