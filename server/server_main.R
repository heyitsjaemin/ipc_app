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
      # selected_state <- reactiveVal("7") # Row 7 : Michigan
      # 
      # # Ensure RATE and DEATHS columns are numeric and handle lists
      # merged_state_data <- merged_state_data %>%
      #   mutate(
      #     CRUDE_RATE = as.numeric(unlist(CRUDE_RATE)),  # Flatten list and convert to numeric
      #     DEATHS = as.numeric(unlist(DEATHS))  # Flatten list and convert to numeric
      #   )
      # 
      # #Handle any missing or non-numeric values
      # merged_state_data <- merged_state_data %>%
      #   mutate(
      #     CRUDE_RATE = ifelse(is.na(CRUDE_RATE), 0, CRUDE_RATE),  # Replace NA with 0
      #     DEATHS = ifelse(is.na(DEATHS), 0, DEATHS)  # Replace NA with 0
      #   )
      
      
      # # Update selected_state when a state is clicked
      # observeEvent(input$usa_map_shape_click, {
      #   new_state <- input$usa_map_shape_click$id  # Capture clicked state ID
      #   new_state <- gsub("^X|_1$", "", new_state)
      #   
      #   # Debugging: Print clicked state
      #   cat("Clicked State ID:", new_state, "\n")
      #   
      #   # Ensure new_state is valid before updating
      #   if (!is.na(new_state) && new_state >= 1 && new_state <= 48) {
      #     selected_state(new_state)  # Update reactive value
      #     cat("Updated Selected State:", selected_state(), "\n")  # Debugging
      #   } else {
      #     cat("Invalid state clicked!\n")
      #   }
      # })
      # 
      # # Create a reactive expression for fetching the selected state's name (NAME)
      # selected_state_name <- reactive({
      #   row_num <- selected_state()  # Now this is the row number
      #   state_data <- merged_state_data[row_num, ]  # Access the row directly by index
      #   return(state_data$NAME)
      # })
      # 
      # # Create a reactive expression for fetching the selected state's overdose CRUDE_RATE (CRUDE_RATE)
      # selected_state_rate <- reactive({
      #   row_num <- selected_state()  # Now this is the row number
      #   state_data <- merged_state_data[row_num, ]  # Access the row directly by index
      #   return(state_data$CRUDE_RATE)
      # })
      # 
      # # Create a reactive expression for fetching the selected state's death tolls (DEATHS)
      # selected_state_death <- reactive({
      #   row_num <- selected_state()  # Now this is the row number
      #   state_data <- merged_state_data[row_num, ]  # Access the row directly by index
      #   return(state_data$DEATHS)
      # })
      # 
      # # Create a reactive expression for fetching the selected state's population (POPULATION)
      # selected_state_pop <- reactive({
      #   row_num <- selected_state()  # Now this is the row number
      #   state_data <- merged_state_data[row_num, ]  # Access the row directly by index
      #   return(state_data$POPULATION)
      # })
      
      # # Sample data for the 2x4 table
      # table_data <- reactive({
      #   row_num <- selected_state()  # Now this is the row number
      #   state_data <- merged_state_data[row_num, ]  # Access the row directly by index
      #   
      #   data.frame(
      #     Field = c("National Average", "State", "Crude Death Rate", "Total Deaths", "Total Population"),
      #     Value = c(formatC(avg_crude_rate, format = "f", big.mark = ",", digits = 2), 
      #               state_data$NAME, 
      #               formatC(state_data$CRUDE_RATE, format = "f", big.mark = ",", digits = 2), 
      #               formatC(state_data$DEATHS, format = "f", big.mark = ",", digits = 0), 
      #               formatC(state_data$POPULATION, format = "f", big.mark = ",", digits = 0))
      #   )
      # })
      # 
      # # Render the table
      # output$my_table <- renderTable({
      #   table_data()
      # })
      # 
      # observeEvent(input$level,{
      #   req(input$level)
      #   
      #   if (input$level == "state"){
      #     req(merged_state_data)
      #     selected_state("7")
      #     
      #     # Render the map
      #     output$usa_map <- renderTmap({
      #       
      #       
      #       data <- reactive_state_data()
      #       
      #       # Check if the user selected Hotspot Analysis
      #       if (input$map_type == "Hotspot Analysis") {
      #         tm_shape(data) +
      #           tm_borders() +
      #           tm_fill(
      #             col = "hotspot_category",  # Use Hotspot Classification
      #             palette = c("blue", "white", "red"),  # Coldspot, Neutral, Hotspot
      #             title = "Hotspot Analysis",
      #             popup.vars = c("State" = "NAME", "Hotspot Category" = "hotspot_category")
      #           ) +
      #           tm_layout(
      #             scale = 1.5,
      #             legend.title.size = 1.2
      #           )
      #         
      #       } else {  # Standard Mode
      #         tm_shape(data) +
      #           tm_borders() +
      #           tm_fill(
      #             col = "CRUDE_RATE",
      #             palette = "brewer.blues",
      #             style = "quantile",
      #             title = "Crude Rate",
      #             popup.vars = c("State" = "NAME", "Crude Rate" = "CRUDE_RATE", "Deaths" = "DEATHS")
      #           ) +
      #           tm_layout(
      #             scale = 1.5,
      #             legend.title.size = 1.2
      #           )
      #       }
      #       
      #       
      #       
      #     })
      #   }
      # })
      # 
      # output$scatter_stats <- renderUI({
      #   req(input$scatter_var)  # Ensure input is not null
      #   
      #   # Remove total row
      #   state_data <- overdose_state[overdose_state$STATE != "Total", ]
      #   
      #   # Convert columns to numeric
      #   state_data$POPULATION <- as.numeric(state_data$POPULATION)
      #   state_data$DEATHS <- as.numeric(state_data$DEATHS)
      #   state_data$CRUDE_RATE <- as.numeric(state_data$CRUDE_RATE)
      #   
      #   # Get selected variable
      #   selected_var <- switch(input$scatter_var,
      #                          "Population" = state_data$POPULATION,
      #                          "Deaths" = state_data$DEATHS)
      #   
      #   # Compute correlation and regression
      #   regression_model <- lm(CRUDE_RATE ~ selected_var, data = state_data)
      #   correlation_value <- cor(selected_var, state_data$CRUDE_RATE, use = "complete.obs")
      #   r_squared <- summary(regression_model)$r.squared
      #   slope <- coef(regression_model)[2]
      #   
      #   # Generate formatted output with unique id's for tooltips
      #   tagList(
      #     tags$p(HTML(paste0("<span id='slope_info'><b>📈 Slope:</b></span> ", round(slope, 4)))),
      #     tags$p(HTML(paste0("<span id='r_squared_info'><b>📉 R-squared (R²):</b></span> ", round(r_squared, 4)))),
      #     tags$p(HTML(paste0("<span id='correlation_info'><b>🔗 Correlation Coefficient (Pearson’s r):</b></span> ", round(correlation_value, 4)))),
      #     tags$p(HTML(paste0("<span id='interpretation_info'><b>💡 Interpretation:</b></span> ", 
      #                        ifelse(abs(correlation_value) > 0.7, "Strong correlation.",
      #                               ifelse(abs(correlation_value) > 0.3, "Moderate correlation.", "Weak or no correlation.")))))
      #   )
      # })
      # 
      # 
      # observe({
      #   req(input$scatter_var)  # Ensure a variable is selected before running tooltips
      #   
      #   bsTooltip(id = "slope_info", title = "The slope shows how much the Crude Rate changes when the selected variable increases by one unit.", placement = "right")
      #   bsTooltip(id = "r_squared_info", title = "R² indicates how well the selected variable explains the variation in Crude Rate. A value closer to 1 means a strong relationship.", placement = "right")
      #   bsTooltip(id = "correlation_info", title = "Pearson's r measures the strength of the relationship. Closer to +1 or -1 means a strong correlation.", placement = "right")
      #   bsTooltip(id = "interpretation_info", title = "If the correlation is weak, other external factors might be affecting the Crude Rate.", placement = "right")
      # })
      # 
      # 
      # 
      # 
      # output$scatter_plot <- renderPlot({
      #   # Remove total row
      #   state_data <- overdose_state[overdose_state$STATE != "Total", ]
      #   
      #   # Convert columns to numeric
      #   state_data$POPULATION <- as.numeric(state_data$POPULATION)
      #   state_data$DEATHS <- as.numeric(state_data$DEATHS)
      #   state_data$CRUDE_RATE <- as.numeric(state_data$CRUDE_RATE)
      #   
      #   # Get selected variable
      #   selected_var <- switch(input$scatter_var,
      #                          "Population" = state_data$POPULATION,
      #                          "Deaths" = state_data$DEATHS)
      #   
      #   # Create scatterplot
      #   ggplot(state_data, aes(x = selected_var, y = CRUDE_RATE)) +
      #     geom_point(color = "#0073e6", size = 4, alpha = 0.8) +
      #     geom_smooth(method = "lm", color = "red", linetype = "dashed") +  # Trend line
      #     scale_x_log10() +  # Log scale for better readability
      #     labs(title = paste("Crude Rate vs.", input$scatter_var, "(State Level)"),
      #          x = paste(input$scatter_var, "(Log Scale)"), 
      #          y = "Crude Rate") +
      #     theme_minimal() +
      #     theme(
      #       plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      #       axis.title = element_text(face = "bold"),
      #       panel.grid.major = element_line(color = "grey80"),
      #       plot.background = element_rect(fill = "#f8f9fa", color = NA),
      #       panel.background = element_rect(fill = "#ffffff", color = "#dddddd")
      #     )
      # })
      # 
      # reactive_state_data <- reactive({
      #   req(input$map_type)  # Ensure input exists
      #   
      #   if (input$map_type == "Hotspot Analysis") {
      #     data <- compute_hotspot(merged_state_data)
      #     if (!"hotspot_category" %in% colnames(data)) {
      #       stop("Error: 'hotspot_category' column missing after hotspot computation.")
      #     }
      #     return(data)
      #   } else {
      #     return(merged_state_data)
      #   }
      # })
      
      
      # Handle radio toggle
      observeEvent(input$level, {
        level_name(input$level)
        cat("Level changed to:", level_name(), "\n")
      })
      
      # Load base shapefile for US states
      base_states <- reactive({
        req(usa_states)
        cat("✅ Loaded USA states shapefile\n")
        usa_states
      })
      
      observe({
        updateSelectInput(session, "selected_period", selected = "2023")
        updateSelectInput(session, "var", selected = "Unintentional Drug Overdose Death Rate")
      })
      
      # Map input variable name to database field name
      intent_mapping <- c(
        "Unintentional Drug Overdose Death Rate" = "Drug_OD",
        "Firearm" = "FA_Deaths",
        "Suicide" = "All_Suicide",
        "Homicide" = "All_Homicide"
      )
      
      # Filter CDC injury data from PostgreSQL table based on dropdown inputs
      filtered_data <- reactive({
        req(input$selected_period, input$var)
        intent_value <- intent_mapping[[input$var]]
        cat("✅ Filtering data for Year:", input$selected_period, ", Intent:", intent_value, "\n")
        result <- injury_state %>%
          filter(PERIOD == input$selected_period, INTENT == intent_value)
        cat("✅ Filtered rows:", nrow(result), "\n")
        result
      })
      
      # Merge CDC data with shapefile
      merged_injury_state_data <- reactive({
        req(base_states(), filtered_data())
        merged <- left_join(base_states(), filtered_data(), by = c("GEOID" = "GEOID")) %>%
          mutate(
            CRUDE_RATE = as.numeric(CRUDE_RATE),
            DEATHS = as.numeric(DEATHS),
            CRUDE_RATE = ifelse(is.na(CRUDE_RATE), 0, CRUDE_RATE),
            DEATHS = ifelse(is.na(DEATHS), 0, DEATHS)
          )
        cat("✅ Merged dataset rows:", nrow(merged), "\n")
        merged
      })
      
      # Set default state selection (GEOID 26 = Michigan)
      selected_state <- reactiveVal("26")
      
      # Update selected state when clicked
      observeEvent(input$usa_map_shape_click, {
        # This is the row index, NOT the GEOID
        row_index <- as.numeric(gsub("^X|_1$", "", input$usa_map_shape_click$id))
        
        data <- merged_injury_state_data()
        
        # Make sure the index is valid
        if (!is.na(row_index) && row_index >= 1 && row_index <= nrow(data)) {
          clicked_geoid <- as.character(data$GEOID[row_index])  # Now get the correct GEOID
          selected_state(clicked_geoid)  # Update reactive GEOID
          cat("✅ Clicked GEOID:", clicked_geoid, "\n")
        } else {
          cat("⚠️ Invalid row index clicked:", row_index, "\n")
        }
      })
      
      
      
      # Summary table with full state info
      table_data <- reactive({
        data <- merged_injury_state_data()
        state_row <- data %>%
          filter(GEOID == as.numeric(selected_state()), PERIOD == input$selected_period)
        
        
        # Parse numeric fields
        crude_rate <- as.numeric(state_row$CRUDE_RATE)
        deaths <- as.numeric(state_row$DEATHS)
        cat(crude_rate)
        cat(deaths)
        
        # Estimate population
        estimated_population <- round((deaths * 100000) / crude_rate)
        cat(estimated_population)
        
        # National average
        national_avg <- mean(data$CRUDE_RATE[data$PERIOD == input$selected_period], na.rm = TRUE)
        
        # Build summary table
        data.frame(
          Field = c("National Average", "State", "Crude Death Rate", "Total Deaths", "Total Population"),
          Value = c(
            formatC(national_avg, format = "f", digits = 2),
            state_row$NAME,
            formatC(crude_rate, format = "f", digits = 2),
            formatC(deaths, format = "f", big.mark = ",", digits = 0),
            formatC(estimated_population, format = "f", big.mark = ",", digits = 0)
          )
        )
      })
      
      output$my_table <- renderTable({
        req(table_data())
        table_data()
      })
      
      # Map output
      output$usa_map <- renderTmap({
        data <- merged_injury_state_data()
        cat("✅ Rendering map with", nrow(data), "features\n")
        
        if (input$map_type == "Hotspot Analysis") {
          data <- compute_hotspot(data)
          tm_shape(data) +
            tm_borders() +
            tm_fill(
              col = "hotspot_category",
              palette = c("blue", "white", "red"),
              title = "Hotspot Analysis",
              popup.vars = c("State" = "NAME", "Hotspot" = "hotspot_category")
            ) +
            tm_layout(scale = 1.5)
        } else {
          tm_shape(data) +
            tm_borders() +
            tm_fill(
              col = "CRUDE_RATE",
              palette = "Blues",
              style = "quantile",
              title = "Crude Rate",
              popup.vars = c("State" = "NAME", "Rate" = "CRUDE_RATE", "Deaths" = "DEATHS")
            ) +
            tm_layout(scale = 1.5)
        }
      })
      
      output$scatter_stats <- renderUI({
        req(input$scatter_var)  # Ensure input is not null

        # Remove total row
        state_data <- overdose_state[overdose_state$STATE != "Total", ]

        # Convert columns to numeric
        state_data$POPULATION <- as.numeric(state_data$POPULATION)
        state_data$DEATHS <- as.numeric(state_data$DEATHS)
        state_data$CRUDE_RATE <- as.numeric(state_data$CRUDE_RATE)
        

        # Get selected variable
        selected_var <- switch(input$scatter_var,
                               "Mean Temperature" = state_data$POPULATION,
                               "Precipitation" = state_data$DEATHS)

        # Compute correlation and regression
        regression_model <- lm(CRUDE_RATE ~ selected_var, data = state_data)
        correlation_value <- cor(selected_var, state_data$CRUDE_RATE, use = "complete.obs")
        r_squared <- summary(regression_model)$r.squared
        slope <- coef(regression_model)[2]

        # Generate formatted output with unique id's for tooltips
        tagList(
          tags$p(HTML(paste0("<span id='slope_info'><b>📈 Slope:</b></span> ", round(slope, 4)))),
          tags$p(HTML(paste0("<span id='r_squared_info'><b>📉 R-squared (R²):</b></span> ", round(r_squared, 4)))),
          tags$p(HTML(paste0("<span id='correlation_info'><b>🔗 Correlation Coefficient (Pearson’s r):</b></span> ", round(correlation_value, 4)))),
          tags$p(HTML(paste0("<span id='interpretation_info'><b>💡 Interpretation:</b></span> ",
                             ifelse(abs(correlation_value) > 0.7, "Strong correlation.",
                                    ifelse(abs(correlation_value) > 0.3, "Moderate correlation.", "Weak or no correlation.")))))
        )
      })


      observe({
        req(input$scatter_var)  # Ensure a variable is selected before running tooltips

        bsTooltip(id = "slope_info", title = "The slope shows how much the Crude Rate changes when the selected variable increases by one unit.", placement = "right")
        bsTooltip(id = "r_squared_info", title = "R² indicates how well the selected variable explains the variation in Crude Rate. A value closer to 1 means a strong relationship.", placement = "right")
        bsTooltip(id = "correlation_info", title = "Pearson's r measures the strength of the relationship. Closer to +1 or -1 means a strong correlation.", placement = "right")
        bsTooltip(id = "interpretation_info", title = "If the correlation is weak, other external factors might be affecting the Crude Rate.", placement = "right")
      })




      output$scatter_plot <- renderPlot({
        # Remove total row
        state_data <- overdose_state[overdose_state$STATE != "Total", ]

        # Convert columns to numeric
        state_data$POPULATION <- as.numeric(state_data$POPULATION)
        state_data$DEATHS <- as.numeric(state_data$DEATHS)
        state_data$CRUDE_RATE <- as.numeric(state_data$CRUDE_RATE)

        # Get selected variable
        selected_var <- switch(input$scatter_var,
                               "Mean Temperature" = state_data$POPULATION,
                               "Precipitation" = state_data$DEATHS,
                               NULL)
        
        if (is.null(selected_var)) {
          showNotification("Invalid variable selected for scatter plot.", type = "error")
          return(NULL)
        }
        

        # Create scatterplot
        ggplot(state_data, aes(x = selected_var, y = CRUDE_RATE)) +
          geom_point(color = "#0073e6", size = 4, alpha = 0.8) +
          geom_smooth(method = "lm", color = "red", linetype = "dashed") +  # Trend line
          scale_x_log10() +  # Log scale for better readability
          labs(title = paste("Crude Rate vs.", input$scatter_var, "(State Level)"),
               x = paste(input$scatter_var, "(Log Scale)"),
               y = "Crude Rate") +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.title = element_text(face = "bold"),
            panel.grid.major = element_line(color = "grey80"),
            plot.background = element_rect(fill = "#f8f9fa", color = NA),
            panel.background = element_rect(fill = "#ffffff", color = "#dddddd")
          )
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
          
          # Replace NA values in CRUDE_RATE where POPULATION is missing
          filtered_counties$CRUDE_RATE <- ifelse(is.na(filtered_counties$POPULATION) | filtered_counties$POPULATION == 0, 
                                                 NA,  # Assign NA so missing values don't break the color scale
                                                 filtered_counties$CRUDE_RATE)
          
          # Calculate percentiles dynamically from available data
          quantile_values <- quantile(filtered_counties$CRUDE_RATE, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)
          
          # Ensure bins are unique (prevent errors with identical breaks)
          quantile_values <- unique(quantile_values)
          
          # Define color palette using dynamic quantile bins
          color_palette <- colorBin(palette = "Blues", 
                                    domain = filtered_counties$CRUDE_RATE, 
                                    bins = quantile_values, 
                                    na.color = "gray")
          
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
                "<b>Crude Rate:</b> ", ifelse(is.na(CRUDE_RATE), "Not Available",
                                              ifelse(CRUDE_RATE == -1.0, "Unreliable",
                                                     formatC(CRUDE_RATE, format = "f", big.mark = ",", digits = 2))), "<br>",
                "<b>Deaths:</b> ", formatC(DEATHS, format = "f", big.mark = ",", digits = 0)
              )
            ) %>%
            addLegend(
              position = "bottomright",
              pal = color_palette,
              values = filtered_counties$CRUDE_RATE,
              title = "Crude Death Rate",
              opacity = 1,
              na.label = "Not Available",
              labFormat = labelFormat(digits = 2)  # Show actual values in legend
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
        
        # Check if population is missing
        is_population_missing <- is.na(county_data$POPULATION) | county_data$POPULATION == "Not Available"
        
        # Determine Crude Rate Display
        crude_rate_value <- ifelse(is_population_missing, 
                                   "Not Available",  # If population is missing, crude rate should be NA
                                   ifelse(county_data$CRUDE_RATE == -1.0, 
                                          "Unreliable", 
                                          formatC(county_data$CRUDE_RATE, format = "f", big.mark = ",", digits = 2)))
        
        # Determine Population Display
        population_value <- ifelse(is_population_missing, 
                                   "Not Available", 
                                   formatC(as.numeric(county_data$POPULATION), format = "f", big.mark = ",", digits = 0))
        
        data.frame(
          Field = c("National Average", "County", "Crude Death Rate", "Total Deaths", "Total Population"),
          Value = c(formatC(avg_crude_rate, format = "f", big.mark = ",", digits = 2), 
                    county_data$NAME, 
                    crude_rate_value,  # Uses the fixed logic for Crude Rate
                    formatC(county_data$DEATHS, format = "f", big.mark = ",", digits = 0), 
                    population_value)  # Uses fixed logic for Population
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