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
overdose_state <- read_delim(paste0(file_path, "ipcapp_030_overdose_by_state_2018_2023.txt"), delim="\t")
overdose_county <- read_delim(paste0(file_path, "ipcapp_031_overdose_by_county_2018_2023.txt"), delim="\t")

# Remove the state abbreviation from the COUNTY column
overdose_county$County <- gsub(", [A-Z]{2}$", "", overdose_county$County)
overdose_county <- overdose_county %>%
  mutate(CRUDE_RATE = ifelse(is.na(`Crude Rate`), -1, `Crude Rate`))

# Load shapefile for the entire USA
usa_states <- states(cb = TRUE, class = "sf")
usa_counties <- counties(cb = TRUE, class = "sf")


# Define states/territories to exclude
exclude_states <- c("American Samoa", "Alaska", "Hawaii", "Commonwealth of the Northern Mariana Islands",
                    "Guam", "Puerto Rico", "United States Virgin Islands", "District of Columbia")

exclude_counties <- overdose_county %>%
  filter(!State %in% exclude_states)
usa_counties <- usa_counties %>%
  filter(!STATE_NAME %in% exclude_states)

# Clean and prepare the dataset
overdose_state <- overdose_state %>%
  rename(STATE = State, DEATHS = Deaths, POPULATION = Population, CRUDE_RATE = `Crude Rate`) %>%
  mutate(CRUDE_RATE = as.numeric(gsub("Unreliable", NA, CRUDE_RATE)),  # Handle non-numeric values
         STATE = trimws(STATE))  # Remove trailing spaces

overdose_county <- exclude_counties %>%
  rename(STATE = State, COUNTY = County, DEATHS = Deaths, POPULATION = Population, CRUDE_RATE = `Crude Rate`) %>%
  mutate(CRUDE_RATE = as.numeric(ifelse(CRUDE_RATE == "Unreliable", "UN", CRUDE_RATE)),  # Handle non-numeric values safely
         STATE = trimws(STATE))  # Remove trailing spaces

# Merge the new dataset with the shapefile (based on state abbreviation)
merged_state_data <- usa_states %>%
  left_join(overdose_state, by = c("NAME" = "STATE")) %>%
  filter(!NAME %in% exclude_states)  # Exclude specified states/territories


merged_county_data <- usa_counties %>%
  left_join(overdose_county, by = c("STATE_NAME" = "STATE", "NAMELSAD" = "COUNTY")) %>%
  filter(!STATE_NAME %in% exclude_states)  # Exclude specified states/territories


# Remove the unwanted columns from merged_state_data
merged_state_data <- merged_state_data %>%
  select(-STATEFP, -STATENS, -AFFGEOID, -GEOID)


merged_county_data <- merged_county_data %>%
  select(-STATEFP, -COUNTYFP, -COUNTYNS, -AFFGEOID, -GEOID)

# Transform CRS for consistency
merged_state_data <- st_transform(merged_state_data, crs = 4326)
merged_county_data <- st_transform(merged_county_data, crs = 4326)

# Extract the "Total" row for national statistics
total_row <- overdose_state %>% filter(Notes == "Total")
avg_crude_rate <- as.numeric(total_row$`CRUDE_RATE`)

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
      background-color: #FFFFFF; 
      color: #000000;  
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
    .title-panel {
      text-align: center;
      font-size: 30px;
      font-weight: bold;
      color: #ffffff;
      background-color: #00274c;
      padding: 30px;
    }
    .update-date {
      text-align: right;
      padding: 20px;
    }
    .radio-toolbar {
        display: flex;
        justify-content: center;
        gap: 15px;
        background-color: #f8fbff;
        padding: 12px;;
    }
    
    .shiny-input-radiogroup {
        display: flex;
        gap: 15px;
    }

    .shiny-input-radiogroup label {
        padding: 10px 20px;
        font-size: 16px;
        font-weight: bold;
        border-radius: 5px;
        cursor: pointer;
        background-color: #e0e0e0;
        border: 2px solid #ccc;
        transition: all 0.3s ease-in-out;
    }
    .shiny-input-radiogroup input[type='radio'] {
        display: none;
    }
    .shiny-input-radiogroup .active {
        background-color: #007BFF;
        color: white;
        border-color: #0056b3;
    }
    .sidebar-layout {
      padding: 20px;
    }
    .sidebar {
      padding: 15px; 
      background-color: #f9f9f9; 
      border-radius: 5px; 
    }
    .main-panel {
      padding-left: 20px; 
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
        "Injury Related Outcome Data"
    ),
    
    
    div(class = "update-date",
      "Last Updated on Feb 3, 2025"    
    ),
    
    div(class = "radio-toolbar",
        radioButtons(
          inputId = "level",
          label = NULL,  # Removes the label
          choices = c("State" = "state", "County" = "county"),
          selected = "state",  # Default selection
          inline = TRUE  # Display options horizontally
        )
    ),
    
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "var",
          label = "Choose a variable to visualize:",
          choices = c("Unintentional Drug Overdose Death Rate", "Firearm", "Suicide", "Drowning"),
          selected = "Unintentional Drug Overdose Death Rate"
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
  
  # Reactive variable to track selected level
  level_name <- reactiveVal("state")  # Default value is "state"
  
  # Observe changes in radio button input and update level_name
  observeEvent(input$level, {
    level_name(input$level)  # Updates level_name to "state" or "county"
    cat("Level changed to:", level_name(), "\n")  # Prints for debugging
  })
  
  observe({
    req(level_name())  # Ensure it's not NULL
    if (level_name() == "state") {
      print("State map selected")
      # Initially, set Michigan as the default state
      selected_state <- reactiveVal("MI")
      
      # Ensure RATE and DEATHS columns are numeric and handle lists
      merged_state_data <- merged_state_data %>%
        mutate(
          CRUDE_RATE = as.numeric(unlist(CRUDE_RATE)),  # Flatten list and convert to numeric
          DEATHS = as.numeric(unlist(DEATHS))  # Flatten list and convert to numeric
        )
      
      # Handle any missing or non-numeric values
      merged_state_data <- merged_state_data %>%
        mutate(
          CRUDE_RATE = ifelse(is.na(CRUDE_RATE), 0, CRUDE_RATE),  # Replace NA with 0
          DEATHS = ifelse(is.na(DEATHS), 0, DEATHS)  # Replace NA with 0
        )
      
      # Create a reactive expression for fetching the selected state's name (NAME)
      selected_state_name <- reactive({
        state_id <- selected_state()
        state_data <- merged_state_data %>% filter(STUSPS == state_id)
        return(state_data$NAME)
      })
      
      # Create a reactive expression for fetching the selected state's overdose CRUDE_RATE (CRUDE_RATE)
      selected_state_rate <- reactive({
        state_id <- selected_state()
        state_data <- merged_state_data %>% filter(STUSPS == state_id)
        return(state_data$CRUDE_RATE)
      })
      
      # Create a reactive expression for fetching the selected state's death tolls (DEATHS)
      selected_state_death <- reactive({
        state_id <- selected_state()
        state_data <- merged_state_data %>% filter(STUSPS == state_id)
        return(state_data$DEATHS)
      })
      
      # Create a reactive expression for fetching the selected state's population (POPULATION)
      selected_state_pop <- reactive({
        state_id <- selected_state()
        state_data <- merged_state_data %>% filter(STUSPS == state_id)
        return(state_data$POPULATION)
      })
      
      # Sample data for the 2x4 table
      table_data <- reactive({
        state_id <- selected_state()
        state_data <- merged_state_data %>% filter(STUSPS == state_id)
        
        data.frame(
          Field = c("National Average", "State", "Crude Death Rate", "Total Deaths", "Total Population"),
          Value = c(formatC(avg_crude_rate, format = "f", big.mark = ",", digits = 2), 
                    state_data$NAME, 
                    formatC(state_data$CRUDE_RATE, format = "f", big.mark = ",", digits = 2), 
                    formatC(state_data$DEATHS, format = "f", big.mark = ",", digits = 0), 
                    formatC(state_data$POPULATION, format = "f", big.mark = ",", digits = 0))
        )
      })
      
      # Render the table
      output$my_table <- renderTable({
        table_data()
      })
      
      # Render the map
      output$usa_map <- renderTmap({
        tm_shape(merged_state_data) +
          tm_borders() +
          tm_fill(
            col = ifelse(input$var == "Overdose Rate", "CRUDE_RATE", "DEATHS"),  # Dynamically choose the column to display
            palette = "YlGn", 
            style = "quantile",
            title = input$var,
            popup.vars = c("State" = "NAME","Crude Rate" = "CRUDE_RATE")
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
      # Add state-level processing code here
    } else {
      
      
      print("County map selected")
      # Initially, set Washtenaw County, Michigan as the default state
      selected_county <- reactiveVal("MI")
      
      # Ensure RATE and DEATHS columns are numeric and handle lists
      merged_county_data <- merged_county_data %>%
        mutate(
          CRUDE_RATE = as.numeric(unlist(CRUDE_RATE)),  # Flatten list and convert to numeric
          DEATHS = as.numeric(unlist(DEATHS))  # Flatten list and convert to numeric
        )
      
      # Handle any missing or non-numeric values
      merged_county_data <- merged_county_data %>%
        mutate(
          CRUDE_RATE = ifelse(is.na(CRUDE_RATE), 0, CRUDE_RATE),  # Replace NA with 0
          DEATHS = ifelse(is.na(DEATHS), 0, DEATHS)  # Replace NA with 0
        )
      
      # Create a reactive expression for fetching the selected state's name (NAME)
      selected_county_name <- reactive({
        county_id <- selected_county()
        county_data <- merged_county_data %>% filter(STUSPS == state_id)
        return(state_data$NAME)
      })
      
      # Create a reactive expression for fetching the selected state's overdose CRUDE_RATE (CRUDE_RATE)
      selected_state_rate <- reactive({
        state_id <- selected_state()
        state_data <- merged_state_data %>% filter(STUSPS == state_id)
        return(state_data$CRUDE_RATE)
      })
      
      # Create a reactive expression for fetching the selected state's death tolls (DEATHS)
      selected_state_death <- reactive({
        state_id <- selected_state()
        state_data <- merged_state_data %>% filter(STUSPS == state_id)
        return(state_data$DEATHS)
      })
      
      # Create a reactive expression for fetching the selected state's population (POPULATION)
      selected_state_pop <- reactive({
        state_id <- selected_state()
        state_data <- merged_state_data %>% filter(STUSPS == state_id)
        return(state_data$POPULATION)
      })
      
      # Sample data for the 2x4 table
      table_data <- reactive({
        state_id <- selected_state()
        state_data <- merged_state_data %>% filter(STUSPS == state_id)
        
        data.frame(
          Field = c("National Average", "State", "Crude Death Rate", "Total Deaths", "Total Population"),
          Value = c(formatC(avg_crude_rate, format = "f", big.mark = ",", digits = 2), 
                    state_data$NAME, 
                    formatC(state_data$CRUDE_RATE, format = "f", big.mark = ",", digits = 2), 
                    formatC(state_data$DEATHS, format = "f", big.mark = ",", digits = 0), 
                    formatC(state_data$POPULATION, format = "f", big.mark = ",", digits = 0))
        )
      })
      
      # Render the table
      output$my_table <- renderTable({
        table_data()
      })
      
      # Render the map
      output$usa_map <- renderTmap({
        tm_shape(merged_state_data) +
          tm_borders() +
          tm_fill(
            col = ifelse(input$var == "Overdose Rate", "CRUDE_RATE", "DEATHS"),  # Dynamically choose the column to display
            palette = "YlGn", 
            style = "quantile",
            title = input$var,
            popup.vars = c("State" = "NAME","Crude Rate" = "CRUDE_RATE")
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
  })
  
  
}

# Run the Shiny app
shinyApp(ui, server)
