# Data cleaning script for "shiny_capstone" app
# Part of the Intro to Shiny course from Posit Academy
# 2024-11-17

# Setup
source("R/imp_clean_funs.R")
library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(vroom) # Read and Write Rectangular Text Data Quickly 
library(rvest) # Easily Harvest (Scrape) Web Pages
library(data.table) # Extension of `data.frame`

# Import
# Direct HTTPS import

https_location <- "https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/"
html <- read_html("https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/")

full_urls <- html %>%  
    html_elements(css = "a") %>%
    html_attr("href") %>%
    .[str_detect(., "details")] %>%
    map_chr(., ~paste0(https_location, .x))



###################################################################
# For local file import
## Files names 
# detail_files <- fs::dir_ls(path = "data/input", glob = "*details*")
###################################################################

# Load files
# storm_details <- vroom(detail_files,  .name_repair = janitor::make_clean_names) 
storm_details <- vroom(full_urls,
                       delim = ",",
                       .name_repair = janitor::make_clean_names)


# Cleaning and selecting for "Storm"

# Making regions to add to initial dataframe
regions <- as.character(datasets::state.region)
states <- datasets::state.name
state_region_map <- setNames(regions, states)

storms <- storm_details %>%
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
        damage_property = str_num_formatter(., "damage_property"),
        damage_crops = str_num_formatter(., "damage_crops"),
        state = str_to_sentence(state),
        region =  case_when(
            state %in% names(state_region_map) ~ state_region_map[state],
            # Correct reference
            TRUE ~ "Unknown"
        )
    ) %>%
    relocate(region, year_fct, .after = state) 
    #slice_sample(n = 1000)


# save(storms, file = "data/output/storms.rda")

