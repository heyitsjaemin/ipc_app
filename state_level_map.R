#######################################################
#######################################################
# Project Name: Rshiny_App
#
# Script Name: state_level_map.R
#
# Script Description: This creates a page when user clicks certain state to look into.
#
# Author: Jaemin Jeon
# Copyright (c) Jaemin Jeon, 2024
# Email:  jjaemin@umich.edu
#
# Date: 2024-01-05
#
#
# Notes:
# http://127.0.0.1:6416/ 
# https://docs.google.com/document/d/1Z3jY6O5alo5CAnI_WzFExaNsg0lEMiaPeREQvnpev5Q/edit?tab=t.0
#######################################################


library(shiny)
library(tigris)
library(tmap)
library(sf)
library(dplyr)
library(readr)

# Load pre-downloaded data from a CSV file
file_path <- "data/"
overdose_data <- read_csv(paste0(file_path, "ipcapp_020_mi_overdose_norm_2020_2021.csv"))

# Clean up the column names for easier use
colnames(overdose_data) <- c("County_Name", "20totalDispensed", "21totalDispensed", "20pop" , "21pop", "20dispensedPerPop", "21dispensedPerPop")

# Convert the population columns to numeric (removing commas)
overdose_data <- overdose_data %>%
  mutate(
    Data_2020 = as.numeric(gsub(",", "", `20dispensedPerPop`)),
    Data_2021 = as.numeric(gsub(",", "", `21dispensedPerPop`))
  )

# Load shapefile for the selected state (Michigan in this case)
county_shapefile <- counties(state = "MI", class = "sf")

# Get the list of county names in Michigan
county_list <- county_shapefile$NAME

# Load water areas for each county and combine (filtering out NULL results)
water_areas <- lapply(county_list, function(county_name) {
  area <- area_water(state = "MI", county = county_name, class = "sf")
  if (!is.null(area)) return(area)
})

# Remove any NULL elements from the list
water_areas <- do.call(rbind, water_areas[!sapply(water_areas, is.null)])

# Ensure both county and water layers are using the same CRS
water_areas <- st_transform(water_areas, st_crs(county_shapefile))

# Merge the overdose data with the state-level shapefile
merged_state_data <- county_shapefile %>%
  left_join(overdose_data, by = c("NAME" = "County_Name"))

# Transform CRS if needed (optional but recommended for consistency)
merged_state_data <- st_transform(merged_state_data, crs = 4326)

# UI for State-level map
ui <- fluidPage(
  titlePanel("State-Level Map of Injury Related Outcomes"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "var",
        label = "Choose a variable to visualize:",
        choices = c("Overdose Rate", "Death Count"),
        selected = "Overdose Rate"
      )
    ),
    mainPanel(
      tmapOutput("state_map")  # Placeholder for the map only
    )
  )
)

# Server for State-level map
server <- function(input, output, session) {
  # Render the state-level map
  output$state_map <- renderTmap({
    tm_shape(merged_state_data) +
      tm_borders() +
      tm_fill(
        col = ifelse(input$var == "Overdose Rate", "RATE", "DEATHS"),
        palette = "YlGn",
        style = "quantile",
        title = input$var
      ) +
      tm_text(
        text = "COUNTY",
        size = 0.5,
        col = "black"
      ) +
      tm_layout(scale = 1.5)
  })
}

# Run the State-level map page
shinyApp(ui, server)
