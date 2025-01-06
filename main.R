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
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(dplyr)
library(sf) # For spatial data
library(DT)

# Load or simulate example data
data <- data.frame(
  county = rep(paste("County", 1:10), each = 10),
  outcome = sample(c("Drowning", "Road Accidents", "Overdoses"), 100, replace = TRUE),
  count = sample(1:100, 100, replace = TRUE),
  population = sample(1000:10000, 100, replace = TRUE),
  mean_temperature = runif(100, 20, 100),
  year = rep(2010:2019, times = 10)
)

# Example spatial data (replace with real county shapefile)
counties <- st_as_sf(data.frame(
  county = paste("County", 1:10),
  geometry = st_sfc(
    lapply(1:10, function(x) st_point(c(runif(1, -100, -90), runif(1, 30, 40))))
  )
))

ui <- dashboardPage(
  dashboardHeader(title = "Injury Public Health Dashboard"),
  dashboardSidebar(
    selectInput("outcome", "Select Outcome:", choices = unique(data$outcome), selected = "Drowning"),
    checkboxInput("normalize", "Normalize by Population", FALSE),
    menuItem("Map", tabName = "map", icon = icon("map")),
    menuItem("Summary", tabName = "summary", icon = icon("chart-bar")),
    menuItem("Scatterplot", tabName = "scatter", icon = icon("chart-scatter"))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              leafletOutput("map")),
      tabItem(tabName = "summary",
              dataTableOutput("summary")),
      tabItem(tabName = "scatter",
              selectInput("predictor", "Select Predictor:", choices = c("mean_temperature"), selected = "mean_temperature"),
              plotOutput("scatterplot"))
    )
  )
)

server <- function(input, output, session) {
  # Reactive filtered data
  filtered_data <- reactive({
    data %>% filter(outcome == input$outcome)
  })
  
  # Map output
  output$map <- renderLeaflet({
    map_data <- filtered_data() %>%
      group_by(county) %>%
      summarize(count = sum(count), population = mean(population))
    
    if (input$normalize) {
      map_data <- map_data %>% mutate(rate = count / population * 100000)
    }
    
    counties_joined <- counties %>%
      left_join(map_data, by = "county")
    
    leaflet(counties_joined) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~st_coordinates(geometry)[, 1],
        lat = ~st_coordinates(geometry)[, 2],
        radius = ~ifelse(input$normalize, rate, count) / 10,
        popup = ~paste(county, "<br>Count:", count, "<br>Rate:", ifelse(input$normalize, round(rate, 2), "N/A"))
      )
  })
  
  # Summary output
  output$summary <- renderDataTable({
    filtered_data() %>%
      group_by(county) %>%
      summarize(
        Total_Count = sum(count),
        Avg_Population = mean(population),
        Rate_Per_100K = sum(count) / mean(population) * 100000
      )
  })
  
  # Scatterplot output
  output$scatterplot <- renderPlot({
    ggplot(filtered_data(), aes_string(x = input$predictor, y = "count")) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(x = input$predictor, y = "Outcome Count", title = paste("Scatterplot of", input$predictor, "vs", input$outcome))
  })
}

shinyApp(ui, server)
