# --- 1. Load Libraries ---
library(tidyverse) # Core suite for data wrangling (dplyr, ggplot2, etc.)
library(sf)        # For simple features (spatial data) manipulation
library(openxlsx)  # For reading/writing Excel files (included but unused in the current script)
library(DBI)       # Standard R interface for connecting to databases
library(RSQLite)   # DBI driver for SQLite databases
library(dbplyr)    # For generating SQL queries from dplyr verbs

# Assumes 'functions.R' contains the definition for 'get_scor_data_df'
source("functions.R")

# --- 2. Configuration & Setup ---
# Define database file and table names using constants
DB_FILE_PATH <- "scor_site_data.sqlite"
TABLE_NAME <- "scor_data"
SITE_TABLE_NAME <- "Sites"

# Connect to (or create) the SQLite database file
# The connection object 'con' is used for all subsequent database operations
con <- DBI::dbConnect(RSQLite::SQLite(), DB_FILE_PATH)
message(paste("Database connection established for:", DB_FILE_PATH))

# Define the end time for data retrieval, set to the start of the next day
END_TIME <- Sys.time() |>
  # Ceiling_date needs lubridate, which is typically loaded with tidyverse,
  # but using a base R approach or explicitly loading lubridate is safer
  # Refined using base R and format for simplicity, assuming date manipulation
  # for 'ceiling' isn't strictly necessary if data is fetched up to 'now'
  # Reverting to original logic but ensuring formatting is done last
  lubridate::ceiling_date(Sys.time(), unit = "day") |>
  format("%Y-%m-%d %H:%M:%S")

# Define a generic start time for sites with no prior data
GENERIC_START_TIME <- "2025-01-01 00:00:00"

# --- 3. Retrieve and Update Site Geometry Data ---
# Explicitly construct the URL to query the layer and request GeoJSON output
# GeoJSON output is preferred for st_read as it is often faster and more direct
feature_query_url <- "https://services3.arcgis.com/iQDjAnwEnrAur0g1/arcgis/rest/services/scor_deployed_sites_w_targettype/FeatureServer/0/query?where=1=1&outFields=*&f=geojson"

print("Attempting to read new site data using explicit GeoJSON query...")
# Read site features directly into an sf object
scor_sites_sf <- st_read(feature_query_url)

# Read existing site names from the database 'Sites' table
sites_in_db <- st_read(con, SITE_TABLE_NAME) |>
  st_drop_geometry() |> # Drop geometry since only SITENAME is needed
  select(SITENAME) |>
  distinct() |>
  pull(SITENAME) # Extract SITENAME as a character vector

# Write *only* new sites to the 'Sites' table in the database
# Filter out sites already present in the database (SITENAME not in sites_in_db)
st_write(
  scor_sites_sf |> filter(!(SITENAME %in% sites_in_db)),
  con,
  SITE_TABLE_NAME, # Use the defined constant
  delete_layer = FALSE, # Do not delete the existing table
  append = TRUE # Append new rows to the existing table
)

# --- 4. Determine Data Fetch Time Windows ---
# Create a tibble of all unique site names (including newly added ones)
# Combine old and new site names for a complete list
all_site_names <- scor_sites_sf |>
  st_drop_geometry() |>
  select(SITENAME) |>
  distinct() |>
  # Ensure the resulting tibble uses the column name expected downstream
  rename(SITE = SITENAME) |>
  as_tibble()

# Reference the main data table in the database using dbplyr
data_tbl <- tbl(con, TABLE_NAME)

# Get the maximum (most recent) DATETIME for each SITE from the database
# This defines the starting point for the new data fetch
starttimes_db <- data_tbl |>
  group_by(SITE) |>
  summarise(START_TIME = max(DATETIME, na.rm = TRUE) ) |>
  collect() |> # Bring the results into local memory
  # Format the DATETIME strings consistently for URL use
  mutate(START_TIME = (as_datetime(START_TIME, tz = "UTC") + seconds(1)) |> # Assume UTC for safety
           format("%Y-%m-%d %H:%M:%S"))

# Join the start times back to the full list of sites
sites_to_fetch <- all_site_names |>
  left_join(starttimes_db, by = "SITE") |>
  # Add the common end time for all fetches
  mutate(END_TIME = END_TIME) |>
  # Use the generic start time for sites that have no data yet (START_TIME is NA)
  mutate(START_TIME = coalesce(START_TIME, GENERIC_START_TIME))

# --- 5. Fetch New Data and Upsert to Database ---
# Prepare the data frame for the row-wise function application
data_for_pmap <- sites_to_fetch |>
  select(SITE, START_TIME, END_TIME) |>
  # Rename columns to match the expected arguments of get_scor_data_df
  rename(site_id = SITE,
         start_date = START_TIME,
         end_date = END_TIME)

# Apply the data fetching function row-wise (one call per site/time window)
# pmap_dfr iterates through rows of 'data_for_pmap' and combines the resulting data frames
new_results <- data_for_pmap |>
  pmap_dfr(.f = get_scor_data_df)

# Upsert (update or insert) the new results into the main data table
# 'by' defines the unique key for matching existing rows (SITE and DATETIME)
# 'copy = TRUE' allows dbplyr to handle data upload if needed
# 'in_place = TRUE' performs the operation directly on the database table
# Reference the table with dbplyr
data_tbl %>%
  rows_upsert(new_results, by = c("SITE","DATETIME","Parameter","Units"), copy = TRUE, in_place = TRUE)

# --- 6. Cleanup ---
# Always disconnect from the database when finished to release the connection
DBI::dbDisconnect(con)
message("--- Database connection closed. Task complete. ---")