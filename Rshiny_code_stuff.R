#######################################################
#######################################################
# Project Name:
#
# Script Name:
#
# Script Description:
#
# Author: Peter S. Larson
# Copyright (c) Peter S. Larson, 2024
# Email:  anfangen@umich.edu
#
# Date: 2024-12-03
#
#
# Notes:
#
#######################################################

library(haven)
library(dplyr)
library(tidyverse)
library(tmap)
library(nhdR)
library(sf)
#library(fips)

## Get counties
county_shp <- tigris::counties(state = NULL) %>%
  janitor::clean_names() %>%
  dplyr::mutate(geoid = paste0(str_pad(statefp, 2, pad = "0"), "", str_pad(countyfp, 3, pad = "0"))) %>%
  dplyr::filter(!(statefp %in% c(
    "02", "15", "52", "57", "72", "66", "69", "60"
  )))

## Get great lakes ##
great_lakes <- st_read("data/GIS_data/great_lakes/GL241203_lam.shp")

### Get census data ###
age20 <- get_decennial(geography = "state", 
                       variables = "P13_001N", 
                       year = 2020,
                       sumfile = "dhc")

head(age20)



## Make a master list
master_list <- list()

## Make a list ##
data_list <- list()

path <- c("data/")

year <- 2022

### "Accident" specifics
data_list[[1]] <- read_sas(paste0(path, "data_", year, "/", "accident.sas7bdat")) %>%
  janitor::clean_names() %>%
  dplyr::mutate(dates = as.Date(paste0(year, "-", month, "-", day))) %>%
  dplyr::mutate(geoid = paste0(str_pad(state, 2, pad = "0"), "", str_pad(county, 3, pad = "0")))

## Persons involved
data_list[[2]] <- read_sas(paste0(path, "data_", year, "/", "per_aux.sas7bdat")) %>%
  janitor::clean_names()

## Vehicle attributes
data_list[[3]] <- read_sas(paste0(path, "data_", year, "/", "veh_aux.sas7bdat")) %>%
  janitor::clean_names()

master_list[[1]] <- data_list


#### Get county summaries ####

counties_time_series <- data_list[[1]] %>%
  dplyr::group_by(dates, geoid) %>%
  tally() %>%
  dplyr::group_by(geoid) %>%
  dplyr::arrange(geoid) %>%
  dplyr::group_by(geoid) %>%
  padr::pad(., interval = "day") %>%
  padr::fill_by_value(., value = 0)

counties <- data_list[[1]] %>%
  dplyr::group_by(geoid) %>%
  tally()

county_shp <- county_shp %>%
  dplyr::left_join(counties)

## Make a map
tm_shape(county_shp, projection = 2163) +
  tm_fill("n", style = "quantile") +
  tm_shape(great_lakes) +
  tm_fill(col = "blue") +
  tm_layout(main.title = "Road accidents 2022 (counts)")

### Get some demographics from the census ###
library(tidycensus)
desired_vars = c(
  all = "P2_001N",
  hisp = "P2_002N",
  white = "P2_005N",
  baa = "P2_006N",
  amin = "P2_007N",
  asian = "P2_008N",
  nhopi = "P2_009N",
  other = "P2_010N",
  multi = "P2_011N"
)
#Passing them through the get_decennial() function:

census_data <- get_decennial(
  geography = "county",
  #state = "NC",
  variables = desired_vars,
  # here is where I am using the list
  summary_var = "P2_001N",
  #creates a column w/'total' variable
  year = 2020,
  sumfile = "pl"
) %>%
  janitor::clean_names()

county_shp2 <- county_shp %>%
  dplyr::left_join(census_data, by = "geoid") %>%
  dplyr::mutate(n_std = 100 * n / summary_value)

## Make a map
road_accident_pop <- tm_shape(county_shp2, projection = 2163) +
  tm_fill("n_std", style = "quantile") +
  tm_shape(great_lakes) +
  tm_fill(col = "blue") +
  tm_layout(main.title = "Road accidents 2022 (percent)")


#### Get time series ####
full_time_series <- data_list[[1]] %>%
  dplyr::group_by(dates) %>%
  tally() %>%
  dplyr::arrange(dates) %>%
  padr::pad(., interval = "day") %>%
  padr::fill_by_value(., value = 0)

plot(ts(full_time_series$n, frequency = 365))






#### Get time series ####