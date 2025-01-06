# RShinyApp: Interactive Map of Injury Related Outcomes

This is a Shiny app that visualizes injury-related outcomes (e.g., overdoses, drownings, road accidents) at the state level in the United States. The map is interactive, and users can hover over states to view the state name and population data.

## Features
- **Interactive Map**: Displays U.S. states and allows users to hover over a state to see additional information.
- **Variable Selection**: Users can select from different variables to visualize (Population, Overdoses, Drowning, Road Accidents).
- **Data Source**: The population data is fetched from the U.S. Census Bureau (2020 Decennial Census) using the `tidycensus` package.

## Requirements
- R (version 4.0 or higher)
- Required R packages:
  - `shiny`
  - `tigris`
  - `tmap`
  - `sf`
  - `dplyr`
  - `tidycensus`
  - `readr`
  
Install the required R packages by running:

```r
install.packages(c("shiny", "tigris", "tmap", "sf", "dplyr", "tidycensus", "readr"))
