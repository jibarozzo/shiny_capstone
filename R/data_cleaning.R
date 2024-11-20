# Data cleaning script for Shiny app input
# 2024-11-17

# Setup
source("R/imp_clean_funs.R")
library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(RCurl) # General Network (HTTP/FTP/...) Client Interface for R
library(vroom) # Read and Write Rectangular Text Data Quickly
library(data.table)

# Import
# Direct HTTPS import
# url <- 'https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/'
# filenames = getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
# filenames = paste(url, strsplit(filenames, "\r*\n")[[1]], sep = "")
# vroom(I(filenames), delim = ",")

# Local file import

# Files names 
detail_files <- fs::dir_ls(path = "data/input", glob = "*details*")
# fatality_files <- fs::dir_ls(path = "data/input", glob = "*fatalities*")
# location_files <- fs::dir_ls(path = "data/input", glob = "*locations*")

# Loaded files
storm_details <- vroom(detail_files,  .name_repair = janitor::make_clean_names) 
# storm_fatalities <- vroom(fatality_files)
# storm_location <- vroom(location_files)


# Cleaning and selecting for "Storm"

# Making regions to add to initial dataframe
regions <- as.character(datasets::state.region)
states <- datasets::state.name
state_region_map <- setNames(regions, states)

storms <- storm_details %>%
    slice_sample(n = 1000) %>%
    filter(str_detect(event_type, 'Storm')) %>%
    select(
        begin_yearmonth,
        end_yearmonth,
        state,
        year,
        month_name,
        event_type,
        injuries_direct:damage_crops
    ) %>%
    mutate(
        year_fct = factor(year, exclude =  ""),
        damage_property = so_formatter(., "damage_property"),
        damage_crops = so_formatter(., "damage_crops"),
        state = str_to_sentence(state),
        region =  case_when(
            state %in% names(state_region_map) ~ state_region_map[state],
            # Correct reference
            TRUE ~ "Unknown"
        )
    ) %>%
    relocate(region, year_fct, .after = state)


save(storms, file = "data/output/storms.rda")
