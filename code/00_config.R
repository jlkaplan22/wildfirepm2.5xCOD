# Load libraries
library(tidyverse)
library(ggplot2)
library(lubridate)
library(stringr)
library(sf)
library(sp)
library(raster)
library(terra)
library(tidycensus)
library(USAboundaries)
library(tigris)
library(purrr)

library(fixest)
library(tictoc)
library(beepr)

# Set paths
if(Sys.getenv("HOME") == "/Users/jordankaplan"){
    local_root_path = "/Users/jordankaplan/Library/CloudStorage/Box-Box/"
} else {
    local_root_path = ""
}

local_box_path = paste0(local_root_path, "wildfirepm2.5xCOD/")
results_dir = paste0(local_box_path, "results/")

# Census API key:
census_api_key("2828600b30d7cbcaf6b2375c40e137c3bef459c7", install=TRUE)

# Load FIPS codes
STATEFP <- 
    read.csv("data_raw/State FIPS codes.csv") %>%
    mutate(FIPS = sprintf("%02s", as.character(FIPS))) %>%
    dplyr::select(FIPS, state = stateabb)

non_CONUS_FIPS <- c("60", "66", "69", "72", "78", "02", "15")

fipsihme <- load(file="data/ihme_fips.rda")

