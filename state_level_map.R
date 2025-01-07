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

# Remove the unwanted columns from merged_data
merged_state_data <- merged_state_data %>%
  select(-STATEFP, -COUNTYFP, -COUNTYNS, -GEOID)

# Transform CRS if needed (optional but recommended for consistency)
merged_state_data <- st_transform(merged_state_data, crs = 4326)

# Calculate the average overdose rate across the country
avg_overdose_rate <- mean(overdose_data$Data_2021, na.rm = TRUE)


# UI for State-level map
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  # Add custom CSS for the title and other page styling
  tags$style(HTML("
    body {
      background-color: #f0f8ff;  /* Light Blue Background */
      color: #333333;  /* Dark Text Color */
    }
    .sidebar {
      background-color: #e0f7fa;  /* Lighter Blue Sidebar */
      padding: 20px;
      border-radius: 10px;
    }
    .main-panel {
      background-color: #ffffff;
      border-radius: 10px;
      padding: 20px;
    }
    .shiny-input-container {
      margin-bottom: 10px;
    }
    .tabset-panel {
      background-color: #e0f7fa;  /* Lighter Blue for active sections */
    }
    
    /* Title styling */
    .title-panel {
      font-size: 36px;
      font-weight: bold;
      color: #00796b;  /* Dark Teal for Titles */
      text-align: center;
      padding: 20px;
      background-color: #ffffff;
      border-radius: 10px;
      box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);
      margin-bottom: 20px;
    }
    
    h4 {
      color: #00796b;  /* Dark Teal for Subtitle */
    }
    table {
      width: 100%;
      border-collapse: collapse;
    }
    table, th, td {
      border: 1px solid #00796b;
    }
    th, td {
      padding: 8px;
      text-align: left;
    }
    th {
      background-color: #00796b;
      color: white;
    }
    td {
      background-color: #e0f7fa;
    }
  ")),
  
  # Styled title for the app
  div(class = "title-panel", 
      "Map of Injury Related Outcomes"
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "var",
        label = "Choose a variable to visualize:",
        choices = c("Opioid Overdose", "Firearm", "Suicide", "Older Adult Falls", "Motor Vehicle Crash", "Adverse Childhood Experiences", "Concussion", "Drowning"),
        selected = "Opioid Overdose"
      ),
      # Add a title for the table
      tags$h4("Summary Statistics"),
      # Add table output below the dropdown
      tableOutput("my_table")  # Placeholder for the table
    ),
    mainPanel(
      tmapOutput("state_map")  # Placeholder for the map only
    )
  )
)

# Server for State-level map
server <- function(input, output, session) {
  # Initially, set Washtenaw as the default county
  selected_county <- reactiveVal("Washtenaw")
  
  # Set tmap mode to interactive
  tmap_mode("view")
  
  # Update selected county based on user interaction with the map
  observe({
    click <- input$state_map_click  # Capture map click events
    if (!is.null(click$id)) {
      selected_county(click$id)  # Update the reactive value for the selected county
    }
  })
  
  # Create a reactive expression for fetching the selected state's name (NAME)
  selected_county_name <- reactive({
    county_id <- selected_county()
    county_data <- merged_state_data %>% filter(NAME == county_id)
    return(county_data$NAME)
  })
  
  # Create a reactive expression for fetching the selected state's overdose rate per population(Data_2021)
  selected_county_rate <- reactive({
    county_id <- selected_county()
    county_data <- merged_state_data %>% filter(NAME == county_id)
    return(county_data$Data_2021)
  })
  
  # Sample data for the table
  table_data <- reactive({
    # Get the overdose rate for the selected county
    name_value <- selected_county_name()
    rate_value <- selected_county_rate()
    
    # Create the table with the updated second row value
    data.frame(
      Field = c("State Average", "County", "Overdose Rate"),
      Value = c(avg_overdose_rate, name_value , rate_value)
    )
  })
  
  output$my_table <- renderTable({
    table_data()
  })
  
  
  # Render the state-level map
  output$state_map <- renderTmap({
    tm_shape(merged_state_data) +
      tm_borders() +
      tm_fill(
        col = "21dispensedPerPop",  # Specify the column to visualize
        palette = "YlGn",
        style = "quantile",
        title = "Total Dispensed per Population"  # Title for the legend
      ) +
      tm_layout(scale = 1.5)
  })
}

# Run the State-level map page
shinyApp(ui, server)
