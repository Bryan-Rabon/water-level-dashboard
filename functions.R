# Load required libraries
library(httr2)
library(jsonlite)
library(dplyr) # Explicitly load dplyr for mutate/select/rename for clarity

#' Fetches SCOR monitoring data for a specific site within a date range.
#'
#' @param site_id A string representing the ID of the monitoring site.
#' @param start_date A string in 'YYYY-MM-DD HH:MM:SS' format for the start of the query.
#' @param end_date A string in 'YYYY-MM-DD HH:MM:SS' format for the end of the query.
#' @return A tibble with columns SITE, DATETIME, and Value, or NULL if the request fails.
get_scor_data_df <- function(site_id, start_date, end_date) {
  
  # --- 1. Request Setup ---
  base_url <- "https://metadata.iriver.cloud/api/v1/scor/"
  # Construct the full URL using paste0, which is generally faster than paste
  full_url <- paste0(base_url, site_id)
  
  # Define the query parameters as a named list
  query_params <- list(
    start = start_date,
    end = end_date
  )
  
  # --- 2. API Call and Error Handling ---
  # Use tryCatch for robust error management in row-wise map/apply operations
  tryCatch({
    # Construct the request pipe
    response <- request(full_url) |>
      # Add query parameters (!!! is used for list unquoting)
      req_url_query(!!!query_params) |>
      # Set a reasonable timeout to prevent hanging connections
      req_timeout(30) |>
      # Execute the request
      req_perform()
    
    # --- 3. Process Successful Response ---
    # Check the status code. httr2 automatically checks for 4xx/5xx on req_perform(),
    # but an explicit check ensures status 200 (OK) for expected output.
    if (resp_status(response) == 200) {
      # Extract the response body as a character string
      json_string <- resp_body_string(response)
      
      # Convert the JSON string to an R list/data frame
      # flatten = TRUE converts nested JSON objects into columns with '.' separators
      data_list <- jsonlite::fromJSON(json_string, flatten = TRUE)
      
      # Data structure expectation:
      # data_list$data is the array of time/value pairs (t and v)
      # data_list$site is the site identifier
      data_df <- data_list$data |>
        # Add the site ID column to the returned data rows
        mutate(SITE = data_list$site) |>
        # Select and rename columns to match the database schema
        select(SITE, DATETIME = t, depth_feet, distance_feet,water_elevation_feet) |>
        # Ensure DATETIME is properly converted (optional, but good practice)
        #mutate(DATETIME = as.POSIXct(DATETIME, tz = "UTC")) |>
        # Ensure 'Value' is numeric (important for data integrity)
        mutate(across(ends_with("_feet"), as.numeric)) |>
        pivot_longer(
          cols = ends_with("_feet"),
          # Split into Parameter and Units
          names_to = c("Parameter", "Units"),
          # Regex: (all text) followed by (an underscore) followed by (text with no underscores)
          names_pattern = "(.*)_(.*)$", 
          values_to = "Result"
        ) |>
        select(SITE, DATETIME, Parameter, Result, Units)
      
      message(paste("Successfully retrieved", nrow(data_df), "records for site:", site_id))
      return(data_df)
      
    } else {
      # Should be caught by req_perform() but included for completeness
      stop(paste("API request failed with status code:", resp_status(response)))
    }
    
    # --- 4. Handle Errors and Warnings ---
  }, error = function(e) {
    # If any error occurs (e.g., connection issue, 404/500 status), log a warning
    # and return NULL, which pmap_dfr can handle (or ignore if the resulting list is empty)
    warning(paste("Failed to fetch data for site:", site_id, "Error:", e$message))
    return(NULL)
  })
}