#######################################################
#######################################################
# Project Name: Rshiny_App
#
# Script Name: Main script
#
# Script Description:
#
# Author: Jaemin Jeon
# Copyright (c) Jaemin Jeon, 2024
# Email:  jjaemin@umich.edu
#
# Date: 2024-12-28
#
#
# Notes:
# https://docs.google.com/document/d/1Z3jY6O5alo5CAnI_WzFExaNsg0lEMiaPeREQvnpev5Q/edit?tab=t.0
#######################################################



library(shiny)
library(tigris)
library(tmap)
library(sf)
library(dplyr)


# Load shapefile for the entire USA
usa_shapefile <- states(class = "sf")

# Exclude Alaska, Hawaii, and other non-mainland U.S. territories
usa_shapefile <- usa_shapefile %>% 
  filter(!NAME %in% c("Alaska", "Hawaii", "District of Columbia"))

# Get U.S. population data using tigris (from the census)
census_data <- tidycensus::get_decennial(
  geography = "state",
  variables = "P001001", # Total population
  year = 2020
)

# Merge census data with shapefile (join by state name)
usa_shapefile <- usa_shapefile %>%
  left_join(census_data, by = c("NAME" = "NAME"))

# Transform CRS if needed (optional but recommended for consistency)
usa_shapefile <- st_transform(usa_shapefile, crs = 4326) # WGS84 CRS for web visualization

# Set tmap mode to interactive
tmap_mode("view")

# UI for Shiny app
ui <- fluidPage(
  titlePanel("Map of County Level Counts of Injury Related Outcomes"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "var",
        label = "Choose a variable to visualize:",
        choices = c("Population", "Overdoses", "Drowning", "Road Accidents"),
        selected = "Population"
      )
    ),
    mainPanel(
      tmapOutput("usa_map") # Placeholder for the map
    )
  )
)

# Server for Shiny app
server <- function(input, output, session) {
  # Render the tmap plot
  output$usa_map <- renderTmap({
    tm_shape(usa_shapefile) +
      tm_borders() +
      tm_fill(
        col = "P001001", # Population variable
        palette = "YlGnBu", # Color palette
        title = "Population"
      ) +
      tm_text(
        text = "NAME", # State name to show
        size = 0.5, 
        col = "black"
      ) +
      tm_layout(scale = 1.5)  # Adjust the zoom factor if necessary
  })
}

# Run the Shiny app
shinyApp(ui, server)
