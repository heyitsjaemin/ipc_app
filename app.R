#######################################################
#######################################################
# Project Name: Rshiny_App
#
# Script Name: Main script
#
# Script Description:
#
# This Shiny app provides an interactive platform to visualize injury-related outcomes across the United States, 
# focusing on overdose rates, road accidents, and drowning statistics. Users can select different variables 
# from a dropdown menu, allowing them to toggle between various datasets for comparison. 
#
# The map is color-coded based on the selected variable, highlighting state-specific data 
# such as overdose rates or total deaths. Clicking on a state updates a table below the map with 
# detailed statistics for that state, including its overdose rate, total deaths, and population. 
# The table automatically updates as the user clicks on different states, 
# offering an easy way to compare statistics across the country.
#
# Author: Jaemin Jeon
# Copyright (c) Jaemin Jeon, 2024
# Email:  jjaemin@umich.edu
#
# Date: 2024-12-28
#
#
# Notes:
#
#
# Reference: https://docs.google.com/document/d/1Z3jY6O5alo5CAnI_WzFExaNsg0lEMiaPeREQvnpev5Q/edit?tab=t.0
# Population Stats Reference: https://www.census.gov/data/tables/time-series/demo/popest/2020s-state-total.html#v2024
#######################################################


library(shiny)
library(tigris)
library(tmap)
library(sf)
library(dplyr)
library(readr)
library(shinyjs)

# Load pre-downloaded data from a CSV file
file_path <- "data/"
overdose_data <- read_csv(paste0(file_path, "ipcapp_011_usa_overdose_2022.csv"))

# Load shapefile for the entire USA
usa_shapefile <- states(class = "sf")

# Define states/territories to exclude
exclude_states <- c("American Samoa", "Hawaii", "Commonwealth of the Northern Mariana Islands",
                    "Guam", "Alaska", "Puerto Rico", "United States Virgin Islands", "District of Columbia")

# Merge the overdose data with the shapefile (based on state abbreviation)
merged_data <- usa_shapefile %>%
  left_join(overdose_data, by = c("STUSPS" = "STATE")) %>%
  filter(!NAME %in% exclude_states, NAME != "District of Columbia")  # Exclude specified states/territories

# Remove the unwanted columns from merged_data
merged_data <- merged_data %>%
  select(-REGION, -DIVISION, -STATEFP, -STATENS, -GEOID)


# Transform CRS if needed (optional but recommended for consistency)
merged_data <- st_transform(merged_data, crs = 4326)  # WGS84 CRS for web visualization

# Calculate the average overdose rate across the country
avg_overdose_rate <- mean(overdose_data$RATE, na.rm = TRUE)

# Set tmap mode to interactive
tmap_mode("view")

# UI for Shiny app
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  
  tags$head(
    tags$style(href="https://fonts.googleapis.com/css2?family=Roboto:wght@400;500&display=swap", rel="stylesheet"),
    
    # Custom CSS for styling the header
    tags$style(HTML("
    body {
      background-color: whites; 
      color: black;  
      font-family: 'Roboto', Arial, sans-serif;
    }
    .container-fluid {
      padding : 0;
    }
    .header-container {
      width: 100%;
      background-color: #ffffff;
      border-bottom: 2px solid #00796b;
      padding: 10px 0l
    }
    .top-bar {
      display: flex;
      justify-content: flex-end;
      font-size: 14px;
      padding: 5px 20px;
      background-color: #f5f5f5;
    }
    .top-bar a {
      color: #003366;
      font-weight: bold;
      margin-left: 15px;
      text-decoration: underline;
    }
    .top-bar a:hover {
      color: #00579c;
    }
    .header {
      display: flex;
      align-items: center;
      justify-content: space-between;
      padding: 30px 20px;
    }
    .logo {
      height: 30px;
    }
    .nav-links {
      display: flex;
      gap: 15px;
    }
    .nav-links a {
      font-family: 'Roboto', Arial, sans-serif;
      text-decoration: none;
      color: #000000;
      font-size: 16px;
    }
  ")),
    
    # Full Header (Top Bar + Navigation)
    div(class = "header-container",
        div(class = "top-bar",
            tags$a(href = "https://injurycenter.umich.edu/about-us/membership/", "Become a Member"),
            tags$a(href = "https://injurycenter.umich.edu/#", "Donate")
        ),
        div(class = "header",
            tags$img(src = "image/templogo.png", class = "logo"),
            div(class = "nav-links",
                tags$a(href = "https://injurycenter.umich.edu/about-us/", "About"),
                tags$a(href = "https://injurycenter.umich.edu/injury-focus-areas/", "Focus Areas"),
                tags$a(href = "https://injurycenter.umich.edu/education/", "Education"),
                tags$a(href = "https://injurycenter.umich.edu/research/", "Research"),
                tags$a(href = "https://injurycenter.umich.edu/services-resources/", "Resources"),
                tags$a(href = "https://injurycenter.umich.edu/events", "Events"),
                tags$a(href = "https://injurycenter.umich.edu/about-us/contact-us/", "Contact Us")
            )
        )
    ),
    
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
        tags$h4("Summary Statistics"),
        tableOutput("my_table")  # Placeholder for the table
      ),
      mainPanel(
        tmapOutput("usa_map")  # Placeholder for the map only
      )
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
  
  # Create a reactive expression for fetching the selected state's name (NAME)
  selected_state_name <- reactive({
    state_id <- selected_state()
    state_data <- merged_data %>% filter(STUSPS == state_id)
    return(state_data$NAME)
  })
  
  # Create a reactive expression for fetching the selected state's overdose rate (RATE)
  selected_state_rate <- reactive({
    state_id <- selected_state()
    state_data <- merged_data %>% filter(STUSPS == state_id)
    return(state_data$RATE)
  })
  
  # Create a reactive expression for fetching the selected state's death tolls (DEATHS)
  selected_state_death <- reactive({
    state_id <- selected_state()
    state_data <- merged_data %>% filter(STUSPS == state_id)
    return(state_data$DEATHS)
  })
  
  # Create a reactive expression for fetching the selected state's population (POPULATION)
  selected_state_pop <- reactive({
    state_id <- selected_state()
    state_data <- merged_data %>% filter(STUSPS == state_id)
    return(state_data$POPULATION)
  })
  
  # Sample data for the 2x4 table
  table_data <- reactive({
    # Get the overdose rate for the selected state
    name_value <- selected_state_name()
    rate_value <- selected_state_rate()
    death_value <- selected_state_death()
    pop_value <- selected_state_pop()
    
    # Create the table with the updated second row value
    data.frame(
      Field = c("National Average", "State", "Overdose Rate", "Total Deaths", "Total Population"),
      Value = c(round(avg_overdose_rate,2), name_value , round(rate_value,2), death_value, pop_value)
    )
  })
  
  # Render the table
  output$my_table <- renderTable({
    table_data()
  })
  
  # Render the map
  output$usa_map <- renderTmap({
    tm_shape(merged_data) +
      tm_borders() +
      tm_fill(
        col = ifelse(input$var == "Overdose Rate", "RATE", "DEATHS"),  # Dynamically choose the column to display
        palette = "YlGn", 
        style = "quantile",
        title = input$var,
        popup.vars = c("NAME","RATE")
      ) +
      tm_text(
        text = "STUSPS",  
        size = 0.5, 
        col = "black"
      ) +
      tm_layout(scale = 1.5) 
  })
  
  

  # Update the selected state when the user clicks on a state
  observeEvent(input$usa_map_shape_click, {
    # Store selected state ID
    selected_state(input$usa_map_shape_click$id)
    
    # If Michigan is clicked, redirect to another Shiny page (state-level page)
    if (trimws(selected_state()) == "MI") {
      updateQueryString("?page=state_level_map", mode = "push")
    }
    
    
  })
}

# Run the Shiny app
shinyApp(ui, server)
