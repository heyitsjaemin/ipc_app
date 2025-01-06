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

# Define states/territories to exclude
exclude_states <- c("American Samoa", "Hawaii", "Commonwealth of the Northern Mariana Islands", 
                    "Guam", "Alaska", "Puerto Rico", "United States Virgin Islands", "District of Columbia")

# Merge the overdose data with the shapefile (based on state abbreviation)
merged_data <- usa_shapefile %>%
  left_join(overdose_data, by = c("STUSPS" = "STATE")) %>%
  filter(!NAME %in% exclude_states, NAME != "District of Columbia")  # Exclude specified states/territories

# Transform CRS if needed (optional but recommended for consistency)
merged_data <- st_transform(merged_data, crs = 4326)  # WGS84 CRS for web visualization

# Calculate the average overdose rate across the country
avg_overdose_rate <- mean(overdose_data$RATE, na.rm = TRUE)

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
      tmapOutput("usa_map")  # Placeholder for the map only
    )
  )
)

# Server for Shiny app
server <- function(input, output, session) {
  # Initially, set Michigan as the default state
  selected_state <- reactiveVal("MI")
  
  # Ensure RATE and DEATHS columns are numeric and handle lists
  merged_data <- merged_data %>%
    mutate(
      RATE = as.numeric(unlist(RATE)),  # Flatten list and convert to numeric
      DEATHS = as.numeric(unlist(DEATHS))  # Flatten list and convert to numeric
    )
  
  # Handle any missing or non-numeric values
  merged_data <- merged_data %>%
    mutate(
      RATE = ifelse(is.na(RATE), 0, RATE),  # Replace NA with 0
      DEATHS = ifelse(is.na(DEATHS), 0, DEATHS)  # Replace NA with 0
    )
  
  # Render the map
  output$usa_map <- renderTmap({
    tm_shape(merged_data) +
      tm_borders() +
      tm_fill(
        col = ifelse(input$var == "Overdose Rate", "RATE", "DEATHS"),  # Dynamically choose the column to display
        palette = "YlGn", 
        style = "quantile",
        title = input$var
      ) +
      tm_text(
        text = "STUSPS",  
        size = 0.5, 
        col = "black"
      ) +
      tm_layout(scale = 1.5)  # Adjust the zoom factor if necessary
  })
  
  # Update the selected state when the user clicks on a state
  observeEvent(input$usa_map_shape_click, {
    selected_state(input$usa_map_shape_click$id)
  })
}

# Run the Shiny app
shinyApp(ui, server)
