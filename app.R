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
library(readr)

# Load pre-downloaded data from a CSV file 
file_path <- "data/"
overdose_data <- read_csv(paste0(file_path, "overdose_stats_2022.csv"))

# Load shapefile for the entire USA
usa_shapefile <- states(class = "sf")

# Check the first few rows of both datasets to ensure correct matching
head(usa_shapefile$STUSPS)
head(overdose_data$STATE)

# Merge the overdose data with the shapefile (based on state abbreviation)
merged_data <- usa_shapefile %>%
  left_join(overdose_data, by = c("STUSPS" = "STATE"))

# Check the merged data to confirm the merge was successful
head(merged_data)

# Transform CRS if needed (optional but recommended for consistency)
merged_data <- st_transform(merged_data, crs = 4326)  # WGS84 CRS for web visualization

# Set tmap mode to interactive
tmap_mode("view")

# UI for Shiny app
ui <- fluidPage(
  titlePanel("Map of Injury Related Outcomes"),
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
      tmapOutput("usa_map")  # Placeholder for the map
    )
  )
)

# Server for Shiny app
server <- function(input, output, session) {
  # Render the tmap plot
  output$usa_map <- renderTmap({
    tm_shape(merged_data) +
      tm_borders() +
      tm_fill(
        col = ifelse(input$var == "Overdose Rate", "RATE", "DEATHS"),  # Dynamically choose the column to display
        palette = "YlGnBu",  # Color palette
        title = input$var
      ) +
      tm_text(
        text = "NAME",  # Display the state name
        size = 0.5, 
        col = "black"
      ) +
      tm_layout(scale = 1.5)  # Adjust the zoom factor if necessary
  })
}

# Run the Shiny app
shinyApp(ui, server)