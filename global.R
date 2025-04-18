library(shiny)
library(tigris)
library(tmap)
library(sf)
library(dplyr)
library(readr)
library(shinyjs)
library(leaflet)
library(ggplot2)
library(shinyBS)
library(spdep)
library(DBI)
library(RPostgres)
library(tidyverse)

# Define global connection variable
con <- NULL

# Try to connect to PostgreSQL safely
tryCatch({
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("PG_DBNAME"),
    host = Sys.getenv("PG_HOST"),
    port = as.integer(Sys.getenv("PG_PORT")),
    user = Sys.getenv("PG_USER"),
    password = Sys.getenv("PG_PASSWORD")
  )
  message("Successfully connected to PostgreSQL.")
}, error = function(e) {
  message("Failed to connect to PostgreSQL: ", e$message)
})



# Function to check database health
check_db_connection <- function(conn) {
  tryCatch({
    if (!is.null(conn) && dbIsValid(conn)) {
      dbGetQuery(conn, "SELECT 1;")
      message("Database connection is healthy.")
      TRUE
    } else {
      warning("Database connection is invalid.")
      FALSE
    }
  }, error = function(e) {
    warning("Error checking DB health: ", e$message)
    FALSE
  })
}

# Load data from PostgreSQL
if (!is.null(con) && dbExistsTable(con, "overdose_by_state")) {
  overdose_state <- dbReadTable(con, "overdose_by_state")
}
if (!is.null(con) && dbExistsTable(con, "overdose_by_county")) {
  overdose_county <- dbReadTable(con, "overdose_by_county")
}
if (!is.null(con) && dbExistsTable(con, "injury_state")) {
  injury_state <- dbReadTable(con, "injury_state")
}
if (!is.null(con) && dbExistsTable(con, "injury_county")) {
  injury_county <- dbReadTable(con, "injury_county")
}



# Filter and clean

# list of irrelevant state names and codes 
states_to_remove <- c("American Samoa", "Alaska", "Hawaii", "Commonwealth of the Northern Mariana Islands",
                      "Guam", "Puerto Rico", "United States Virgin Islands", "District of Columbia")
statecodes_to_remove <- c("02", "11", "15", "60", "66", "69", "72", "78")

# ==================overdose_state==================#
overdose_state <- overdose_state %>%
  rename(STATE = State, DEATHS = Deaths, POPULATION = Population, CRUDE_RATE = `Crude.Rate`) %>%
  mutate(
    CRUDE_RATE = as.numeric(gsub("Unreliable", NA, CRUDE_RATE)),
    STATE = trimws(STATE)
  ) %>%
  filter(!(STATE %in% states_to_remove))

# ==================overdose_county==================#
overdose_county <- overdose_county %>%
  rename(
    STATE = State,
    STATECODE = `State.Code`,
    COUNTY = County,
    COUNTYCODE = `County.Code`,
    DEATHS = Deaths,
    POPULATION = Population,
    CRUDE_RATE = `Crude.Rate`
  ) %>%
  mutate(
    CRUDE_RATE = as.numeric(ifelse(CRUDE_RATE == "Unreliable", -1, CRUDE_RATE)),
    STATE = trimws(STATE),
    COUNTY = gsub(", [A-Z]{2}$", "", COUNTY),
    COUNTY = gsub(" County$", "", COUNTY)
  ) %>%
  filter(!(STATE %in% states_to_remove))
# ==================injury_state==================#
injury_state <- injury_state %>%
  rename(
    STATE = NAME,
    INTENT = Intent,
    PERIOD = Period,
    DEATHS = Count,
    CRUDE_RATE = Rate
  ) %>%
  filter(!(STATE %in% states_to_remove))

# ==================injury_county==================#
injury_county <- injury_county %>%
  rename(
    COUNTY = NAME,
    STATE_GEOID = ST_GEOID,
    STATE = ST_NAME,
    INTENT = Intent,
    PERIOD = Period,
    DEATHS = Count,
    CRUDE_RATE = Rate,
    FLAG_MODELED_RATE = Rate_M,
    RATE_INTERVAL = Rate_M_CI,
    DATE = Data_As_Of
  ) %>%
  mutate(
    CRUDE_RATE = as.numeric(ifelse(CRUDE_RATE == "Unreliable", -999.0, CRUDE_RATE)),
    STATE = trimws(STATE),
    COUNTY = gsub(", [A-Z]{2}$", "", COUNTY),
    COUNTY = gsub(" County$", "", COUNTY)
  ) %>%
  filter(!(STATE %in% states_to_remove))


# Load shapefiles
state_shapefile_path <- "shapefile/cb_2018_us_state_500k/cb_2018_us_state_500k.shp"
usa_states <- st_read(state_shapefile_path) %>%
  st_transform(crs = 4326) %>%
  filter(!(NAME %in% states_to_remove))

county_shapefile_path <- "shapefile/cb_2018_us_county_500k/cb_2018_us_county_500k.shp"
usa_counties <- st_read(county_shapefile_path) %>%
  st_transform(crs = 4326) %>%
  filter(!(STATEFP %in% statecodes_to_remove))

# Merge

# ==================overdose_state==================#
merged_state_data <- usa_states %>%
  left_join(overdose_state, by = c("NAME" = "STATE")) %>%
  select(-STATEFP, -STATENS, -AFFGEOID, -GEOID)


# ==================overdose_county==================#
merged_county_data <- usa_counties %>%
  left_join(overdose_county, by = c("STATEFP" = "STATECODE", "NAME" = "COUNTY")) %>%
  mutate(ROWNUM = row_number()) %>%
  select(-COUNTYNS, -AFFGEOID, -GEOID)

# ==================injury_state==================#
merged_injury_state_date <- usa_states %>%
  left_join(injury_state, by = c("NAME" = "STATE")) %>%
  select(-STATEFP, -STATENS, -AFFGEOID)

# ==================injury_county==================#
merged_injury_county_data <- usa_counties %>%
  left_join(injury_county, by = c("STATEFP" = "STATE_GEOID", "NAME" = "COUNTY")) %>%
  mutate(ROWNUM = row_number()) %>%
  select(-COUNTYNS, -AFFGEOID)


# Compute hotspot function
compute_hotspot <- function(spatial_data) {
  if (!"CRUDE_RATE" %in% colnames(spatial_data)) {
    stop("Error: 'CRUDE_RATE' column not found in dataset.")
  }
  
  coords <- st_centroid(spatial_data) %>% st_coordinates()
  nb <- knearneigh(coords, k = 5) %>% knn2nb()
  listw <- nb2listw(nb, style = "W")
  gi_star <- localG(spatial_data$CRUDE_RATE, listw)
  
  spatial_data$hotspot_score <- gi_star
  spatial_data$hotspot_category <- cut(
    gi_star, breaks = c(-Inf, -1.96, 1.96, Inf),
    labels = c("Cold Spot", "Neutral", "Hot Spot")
  )
  
  return(spatial_data)
}

# Calculate national average from PostgreSQL result
total_row <- overdose_state %>% filter(Notes == "Total")
avg_crude_rate <- as.numeric(total_row$CRUDE_RATE)

# Set map rendering mode
tmap_mode("view")

# Disconnect on app stop
onStop(function() {
  if (!is.null(con) && dbIsValid(con)) {
    dbDisconnect(con)
    message("PostgreSQL connection closed.")
  }
})
