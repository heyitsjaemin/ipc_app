# RShinyApp: Interactive Map of Injury Related Outcomes

This is a Shiny app that visualizes injury-related outcomes such as overdose rates, drownings, and road accidents at the state level in the United States. The app features an interactive map where users can hover over states to view data and select different variables for visualization.

## Features

- **Interactive Map**: Displays U.S. states, allowing users to hover over each state to see detailed information like state name and overdose rate.
- **Variable Selection**: Users can select from various variables to visualize, including:
  - Overdose Rate
  - Road Accidents
  - Drowning
- **Dynamic Summary Statistics**: As users click on states, the app dynamically updates summary statistics, such as the overdose rate and total deaths for the selected state.
- **Data Source**: The data used for overdose rates and deaths is fetched from a pre-loaded CSV file, and the map is created using shapefiles from the U.S. Census Bureau.

## Requirements

- **R Version**: 4.0 or higher
- **Required R Packages**:
  - `shiny` - For building the interactive web app
  - `tigris` - For loading U.S. geographic data (shapefiles)
  - `tmap` - For rendering the interactive map
  - `sf` - For spatial data handling
  - `dplyr` - For data manipulation
  - `readr` - For reading CSV data
  - `shinyjs` - For enhancing interactivity in Shiny
  
## How to Run the App

- Clone or download this repository to your local machine.
- Open RStudio or your preferred R environment.
- Run the app.R script using shinyApp().

To install the required R packages, run the following command in your R console:

```r
install.packages(c("shiny", "tigris", "tmap", "sf", "dplyr", "readr", "shinyjs"))