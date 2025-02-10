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
library(leaflet)

# Define the states that need to be removed
states_to_remove <- c("American Samoa", "Alaska", "Hawaii", "Commonwealth of the Northern Mariana Islands",
                      "Guam", "Puerto Rico", "United States Virgin Islands", "District of Columbia")
statecodes_to_remove <- c("02", "11", "15", "60", "66", "69", "72", "78")

# Load pre-downloaded data from a CSV file
file_path <- "data/"
overdose_state <- read_delim(paste0(file_path, "ipcapp_030_overdose_by_state_2018_2023.txt"), delim="\t")
overdose_state <- overdose_state[1:53, ]  # Keep only the first 53 rows
overdose_state <- overdose_state[!(overdose_state$State %in% states_to_remove), ] # Remove irrelevant states

overdose_county <- read_delim(paste0(file_path, "ipcapp_031_overdose_by_county_2018_2023.txt"), delim="\t")
overdose_county <- overdose_county[1:2287, ]  # Keep only the first 2287 rows
overdose_county <- overdose_county[!(overdose_county$State %in% states_to_remove), ] # Remove irrelevant counties
overdose_county$County <- gsub(", [A-Z]{2}$", "", overdose_county$County) # Remove the state abbreviation from the COUNTY column
overdose_county$County <- gsub(" County$", "", overdose_county$County) # Remove the trailing " County" from the COUNTY column

# Load shapefile for the entire USA
state_shapefile_path <- "shapefile/cb_2018_us_state_500k/cb_2018_us_state_500k.shp"
usa_states <- st_read(state_shapefile_path)
usa_states <- st_transform(usa_states, crs = 4326)
usa_states <- usa_states[!(usa_states$NAME %in% states_to_remove), ] # Remove irrelevant states


county_shapefile_path <- "shapefile/cb_2018_us_county_500k/cb_2018_us_county_500k.shp"
usa_counties <- st_read(county_shapefile_path)
usa_counties <- st_transform(usa_counties, crs= 4326)
usa_counties <- usa_counties[!(usa_counties$STATEFP %in% statecodes_to_remove), ] # Remove irrelevant counties

# Clean and prepare the dataset
overdose_state <- overdose_state %>%
  rename(STATE = State, DEATHS = Deaths, POPULATION = Population, CRUDE_RATE = `Crude Rate`) %>% # Capitalize all the column titles
  mutate(CRUDE_RATE = as.numeric(gsub("Unreliable", NA, CRUDE_RATE)),  # Handle non-numeric values
         STATE = trimws(STATE))  # Remove trailing spaces

overdose_county <- overdose_county %>%
  rename(STATE = State, STATECODE = `State Code`, COUNTY = County, COUNTYCODE = `County Code`, DEATHS = Deaths, POPULATION = Population, CRUDE_RATE = `Crude Rate`) %>% # Capitalize all the column titles
  mutate(CRUDE_RATE = as.numeric(ifelse(CRUDE_RATE == "Unreliable", -1, CRUDE_RATE)),  # Change non-numeric values (Unreliable) to -1.0
         STATE = trimws(STATE))  # Remove trailing spaces

# Merge the new dataset with the shapefile (based on state abbreviation)
merged_state_data <- usa_states %>%
  left_join(overdose_state, by = c("NAME" = "STATE"))


merged_county_data <- usa_counties %>%
  left_join(overdose_county, by = c("STATEFP" = "STATECODE", "NAME" = "COUNTY"))
merged_county_data <- merged_county_data %>%
  mutate(ROWNUM = row_number())  # Add a row number column


# Simplify county geometries (reduces vertex count, speeds up rendering)
#merged_county_data <- st_simplify(merged_county_data, dTolerance = 0.01)

# Remove the unwanted columns from merged_state_data
merged_state_data <- merged_state_data %>%
  select(-STATEFP, -STATENS, -AFFGEOID, -GEOID)


merged_county_data <- merged_county_data %>%
  select(-STATEFP, -COUNTYFP, -COUNTYNS, -AFFGEOID, -GEOID)

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
        text-decoration: underline;
    }
    /* Style the dropdown container */
  .dropdown-container {
      display: flex;
      justify-content: center;  /* Center align */
      align-items: center;
      gap: 20px;  /* Space between dropdowns */
      padding: 15px;
      background-color: #f8fbff;  /* Light background */
      border-radius: 5px;
      width: 100%
  }
  
  /* Style each dropdown */
  .selectize-control {
      width: 100%;
  }
  
  .shiny-input-container {
      font-family: 'Roboto', Arial, sans-serif;
      font-size: 16px;
      font-weight: 500;
  }
  
  /* Dropdown select box */
  .selectize-dropdown,
  .selectize-input {
      border: 2px solid #ccc;
      border-radius: 5px;
      padding: 10px;
      background-color: #fff;
      transition: all 0.3s ease-in-out;
  }
  
  /* Dropdown focus effect */
  .selectize-input:focus {
      border-color: #007BFF;
      box-shadow: 0 0 5px rgba(0, 123, 255, 0.5);
  }
  
  /* Hover effect */
  .selectize-dropdown:hover {
      background-color: #eef6ff;
  }
  
  /* Style dropdown text */
  .selectize-input,
  .selectize-dropdown-content {
      color: #333;
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
    
    div(class = "dropdown-container",
        fluidRow(
          column(4,
                 selectInput(
                   inputId = "demographics",
                   label = "Demographics",
                   choices = c("All Demographics", "Male", "Female","Asian", "Black or African American", "White", 
                               "Hispanic or Latino", "Age < 1 year", "Age 1-4 years", "Age 5-14 years", "Age 15-24 years",
                               "Age 25-34 years", "Age 35-44 years", "Age 45-54 years", "Age 55-64 years", 
                               "Age 65-74 years", "Age 75-84 years", "Age 85+ years" ),
                   selected = "All Demographics"
                 )
          ),
          column(4,
                 selectInput(
                   inputId = "year",
                   label = "Year",
                   choices = c("2023", "2022", "2021", "2020"),
                   selected = "2023"
                 )
          ),
        column(4,
               conditionalPanel(
                   condition = "input.level == 'county'",  # Show only if 'county' is selected
                   selectInput(
                       inputId = "selected_state_on_county_level",
                       label = "State",
                       choices = sort(unique(na.omit(merged_county_data$STATE))),   # Dynamically fetch state names
                       selected = "Michigan"  # Default state
                   )
               )
        )
          
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
      selected_state <- reactiveVal("7") # Row 7 : Michigan
      
      # Ensure RATE and DEATHS columns are numeric and handle lists
      merged_state_data <- merged_state_data %>%
        mutate(
          CRUDE_RATE = as.numeric(unlist(CRUDE_RATE)),  # Flatten list and convert to numeric
          DEATHS = as.numeric(unlist(DEATHS))  # Flatten list and convert to numeric
        )

      #Handle any missing or non-numeric values
      merged_state_data <- merged_state_data %>%
        mutate(
          CRUDE_RATE = ifelse(is.na(CRUDE_RATE), 0, CRUDE_RATE),  # Replace NA with 0
          DEATHS = ifelse(is.na(DEATHS), 0, DEATHS)  # Replace NA with 0
        )
      
      
      # Update selected_state when a state is clicked
      observeEvent(input$usa_map_shape_click, {
        new_state <- input$usa_map_shape_click$id  # Capture clicked state ID
        new_state <- gsub("^X|_1$", "", new_state)
        
        # Debugging: Print clicked state
        cat("Clicked State ID:", new_state, "\n")
        
        # Ensure new_state is valid before updating
        if (!is.na(new_state) && new_state >= 1 && new_state <= 48) {
          selected_state(new_state)  # Update reactive value
          cat("Updated Selected State:", selected_state(), "\n")  # Debugging
        } else {
          cat("Invalid state clicked!\n")
        }
      })
      
      # Create a reactive expression for fetching the selected state's name (NAME)
      selected_state_name <- reactive({
        row_num <- selected_state()  # Now this is the row number
        state_data <- merged_state_data[row_num, ]  # Access the row directly by index
        return(state_data$NAME)
      })
      
      # Create a reactive expression for fetching the selected state's overdose CRUDE_RATE (CRUDE_RATE)
      selected_state_rate <- reactive({
        row_num <- selected_state()  # Now this is the row number
        state_data <- merged_state_data[row_num, ]  # Access the row directly by index
        return(state_data$CRUDE_RATE)
      })
      
      # Create a reactive expression for fetching the selected state's death tolls (DEATHS)
      selected_state_death <- reactive({
        row_num <- selected_state()  # Now this is the row number
        state_data <- merged_state_data[row_num, ]  # Access the row directly by index
        return(state_data$DEATHS)
      })
      
      # Create a reactive expression for fetching the selected state's population (POPULATION)
      selected_state_pop <- reactive({
        row_num <- selected_state()  # Now this is the row number
        state_data <- merged_state_data[row_num, ]  # Access the row directly by index
        return(state_data$POPULATION)
      })
      
      # Sample data for the 2x4 table
      table_data <- reactive({
        row_num <- selected_state()  # Now this is the row number
        state_data <- merged_state_data[row_num, ]  # Access the row directly by index
        
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
      
      observeEvent(input$level,{
        req(input$level)
        
        if (input$level == "state"){
          req(merged_state_data)
          selected_state("7")
          
          # Render the map
          output$usa_map <- renderTmap({
            tm_shape(merged_state_data) +
              tm_borders() +
              tm_fill(
                col = "CRUDE_RATE",
                palette = "brewer.blues",  # Use a proper color scale
                style = "quantile",  # Automatically categorize data
                title = "Crude Rate",
                popup.vars = c("State" = "NAME", "Crude Rate" = "CRUDE_RATE", "Deaths" = "DEATHS")
              ) +
              # tm_text(
              #   text = "STUSPS",
              #   size = 0.5,
              #   col = "black"
              # ) +
              tm_layout(
                scale = 1.5,
                legend.title.size = 1.2
              )
          })
        }
      })
      
      
    } else {
      print("County map selected")
      
      # Initially, set a default county (Washtenaw County)
      selected_county <- reactiveVal("2524")  # Row 2524 : Washtenaw County
      
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
      
      req(merged_county_data)  # Ensure data is loaded before using it
      
      # Update selected_county when a user clicks on a county in the map
      observeEvent(input$usa_map_shape_click, {
        req(input$level == "county")  # Ensure we're in county mode
        req(input$usa_map_shape_click)  # Ensure click event exists
        
        print(input$usa_map_shape_click)  # Debugging: Print entire clicked event data
        
        new_county <- input$usa_map_shape_click$id  # Capture clicked county ID (should be ROWNUM)
        new_county <- gsub("^X|_1$", "", new_county)
        
        cat("Raw Clicked County ROWNUM:", new_county, "\n")  # Debugging print
        
        # Validate that ID is not NULL or NA
        if (is.null(new_county) || is.na(new_county)) {
          cat("No valid county selected\n")
          return()
        }
        
        # Convert new_county to numeric since ROWNUM is a number
        new_county <- as.numeric(new_county)
        
        # Ensure valid row number exists in the dataset
        if (new_county >= 1 && new_county <= nrow(merged_county_data)) {
          selected_county(new_county)  # Update selected county ID
          cat("Updated Selected County ROWNUM:", selected_county(), "\n")
        } else {
          cat("Invalid county clicked!\n")
        }
      })
      
      observeEvent(input$selected_state_on_county_level, {
        req(input$selected_state_on_county_level)  # Ensure state is selected
        
        # Debugging: Print the selected state
        cat("Filtering counties for State:", input$selected_state_on_county_level, "\n")
        
        # Filter counties based on selected state
        filtered_counties <- merged_county_data %>%
          filter(STATE == input$selected_state_on_county_level)
        
        # Debugging: Print number of counties found in the selected state
        cat("Number of Counties Found:", nrow(filtered_counties), "\n")
        
        # Update the map to display only the filtered counties
        output$usa_map <- renderLeaflet({
          req(filtered_counties)  # Ensure data is available
          
          # Check if CRUDE_RATE has enough unique values for colorQuantile()
          num_unique_rates <- length(unique(filtered_counties$CRUDE_RATE[!is.na(filtered_counties$CRUDE_RATE)]))
          
          # Choose a coloring method based on the number of unique values
          if (num_unique_rates > 5) {
            color_palette <- colorQuantile("Blues", filtered_counties$CRUDE_RATE, n = 5)
          } else {
            color_palette <- colorBin("Blues", filtered_counties$CRUDE_RATE, bins = 5)
          }
          
          leaflet(data = filtered_counties) %>%
            addTiles() %>%
            addPolygons(
              fillColor = ~color_palette(CRUDE_RATE),
              weight = 1,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(
                weight = 3,
                color = "#666",
                fillOpacity = 0.9,
                bringToFront = TRUE
              ),
              layerId = ~ROWNUM,  # Assigns each county a unique ID based on its ROWNUM
              popup = ~paste0(
                "<b>County:</b> ", NAME, "<br>",
                "<b>Crude Rate:</b> ", ifelse(CRUDE_RATE == -1.0, "Unreliable", formatC(CRUDE_RATE, format = "f", big.mark = ",", digits = 2)), "<br>",
                "<b>Deaths:</b> ", formatC(DEATHS, format = "f", big.mark = ",", digits = 0)
              )
            ) %>%
            setView(lng = mean(st_coordinates(filtered_counties)[,1]), 
                    lat = mean(st_coordinates(filtered_counties)[,2]), 
                    zoom = 6)  # Adjust zoom to focus on selected state
        })
        
      })
      
      
      
      
      # Create a reactive expression for fetching the selected county's name
      selected_county_name <- reactive({
        row_num <- selected_county()  # Now this is the row number
        county_data <- merged_county_data[row_num, ]  # Access the row directly by index
        return(county_data$NAME)
      })
      
      # Create a reactive expression for fetching the selected county's CRUDE_RATE
      selected_county_rate <- reactive({
        row_num <- selected_county()
        county_data <- merged_county_data[row_num, ]
        return(county_data$CRUDE_RATE)
      })
      
      # Create a reactive expression for fetching the selected county's DEATHS
      selected_county_death <- reactive({
        row_num <- selected_county()
        county_data <- merged_county_data[row_num, ]
        return(county_data$DEATHS)
      })
      
      # Create a reactive expression for fetching the selected county's POPULATION
      selected_county_pop <- reactive({
        row_num <- selected_county()
        county_data <- merged_county_data[row_num, ]
        return(county_data$POPULATION)
      })
      
      # Sample data for the 2x4 table
      table_data <- reactive({
        row_num <- selected_county()
        if (is.null(row_num) || is.na(row_num)) {
          return(data.frame(Field = c("No County Selected"), Value = c("")))
        }
        county_data <- merged_county_data[row_num, ]
        
        # Replace -1.0 with "Unreliable" for CRUDE_RATE
        crude_rate_value <- ifelse(county_data$CRUDE_RATE == -1.0, "Unreliable", 
                                   formatC(county_data$CRUDE_RATE, format = "f", big.mark = ",", digits = 2))
        
        data.frame(
          Field = c("National Average", "County", "Crude Death Rate", "Total Deaths", "Total Population"),
          Value = c(formatC(avg_crude_rate, format = "f", big.mark = ",", digits = 2), 
                    county_data$NAME, 
                    crude_rate_value,
                    formatC(county_data$DEATHS, format = "f", big.mark = ",", digits = 0), 
                    formatC(as.numeric(county_data$POPULATION), format = "f", big.mark = ",", digits = 0))  # Explicit conversion
        )
      })
      
      # Render the table
      output$my_table <- renderTable({
        table_data()
      })
      
      # # Render the county map
      # output$usa_map <- renderTmap({
      #   tm_shape(merged_county_data) +
      #     tm_borders() +
      #     tm_fill(
      #       col = "CRUDE_RATE",
      #       palette = "brewer.blues",  # Use a proper color scale
      #       style = "quantile",  # Automatically categorize data
      #       fill.legend = tm_legend(title = "Crude Rate"),  # Move title here
      #       popup.vars = c("County" = "NAME", "Crude Rate" = "CRUDE_RATE", "Deaths" = "DEATHS")
      #     ) +
      #     # tm_text(
      #     #   text = "NAME",  # Show county names
      #     #   size = 0.4,
      #     #   col = "black"
      #     # ) +
      #     tm_layout(
      #       scale = 1.5
      #     )
      # })
      
      # observeEvent(input$level, {
      #   req(input$level)
      #   
      #   if (input$level == "county") {
      #     req(merged_county_data)  # Ensure data is loaded before rendering
      #     selected_county("2524")
      #     
      #     output$usa_map <- renderLeaflet({
      #       leaflet(data = merged_county_data) %>%
      #         addTiles() %>%
      #         addPolygons(
      #           fillColor = ~colorQuantile("Blues", CRUDE_RATE)(CRUDE_RATE),
      #           weight = 1,
      #           opacity = 1,
      #           color = "white",
      #           dashArray = "3",
      #           fillOpacity = 0.7,
      #           highlightOptions = highlightOptions(
      #             weight = 3,
      #             color = "#666",
      #             fillOpacity = 0.9,
      #             bringToFront = TRUE
      #           ),
      #           layerId = ~ROWNUM,  # Assigns each county a unique ID based on its ROWNUM
      #           popup = ~paste0("<b>County:</b> ", NAME, "<br>",
      #                           "<b>Crude Rate:</b> ", CRUDE_RATE, "<br>",
      #                           "<b>Deaths:</b> ", DEATHS)
      #         ) %>%
      #         setView(lng = -95, lat = 37, zoom = 4)  # Center the map
      #     })
      #   }
      # })
    }
    
  })
}

# Run the Shiny app
shinyApp(ui, server)
