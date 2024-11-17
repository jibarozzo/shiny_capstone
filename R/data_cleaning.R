# Data cleaning script for Shiny app input
# 2024-11-17

# Setup
source("R/import_functions.R")
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
location_files <- fs::dir_ls(path = "data/input", glob = "*locations*")
detail_files <- fs::dir_ls(path = "data/input", glob = "*details*")
fatality_files <- fs::dir_ls(path = "data/input", glob = "*fatalities*")

# Loaded files
storm_location <- vroom(location_files)
storm_details <- vroom(detail_files,  .name_repair = janitor::make_clean_names) # Main data
storm_fatalities <- vroom(fatality_files)

# Cleaning and selecting for "HURRICANE"
tropical_storms <- storm_details %>%
    filter(str_detect(event_type, 'Tropical')) %>%
    select(!dplyr::starts_with("tor")) %>%
    mutate(
        year_fct = as.factor(year),
        damage_property_num = so_inverter(., "damage_property"),
        damage_crops_num = so_inverter(., "damage_crops"),
        episode_id_chr = as.character(episode_id)
    )


tropical_storms |>
    group_by(year) |>
    summarise(sum_damage_crops = sum(damage_crops_num, na.rm = TRUE)) |>
    ggplot() +
    geom_line(aes(x = year, y = sum_damage_crops))


