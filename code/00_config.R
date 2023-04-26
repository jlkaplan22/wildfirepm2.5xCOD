# Load libraries
library(tidyverse)
library(ggplot2)
library(ggbreak)
library(patchwork)
library(gt)
library(webshot2)
library(viridis)
library(lubridate)
library(stringr)
library(sf)
library(sp)
library(raster)
library(terra)
library(tidycensus)
library(USAboundaries)
library(tigris)
options(tigris_use_cache = TRUE) #stores data in case there are temporary issues with site
library(purrr)
library(prism)
library(exactextractr)

library(fixest)
library(tictoc)
library(beepr)
library(tmap)
library(splines)


library(tidylog)

#Get values for hidden pieces of info (API key):
source("code/secrets.R")

# Set paths
if(Sys.getenv("HOME") == "/Users/jordankaplan"){
    local_root_path = "/Users/jordankaplan/Documents/"
} else {
    local_root_path = ""
}

local_box_path = paste0(local_root_path, "wildfirepm2.5xCOD/")
raw_data_dir = paste0(local_box_path, "data_raw/")
data_dir = paste0(local_box_path, "data/")

# Census API key:
census_api_key(API_KEY, install=TRUE, overwrite=TRUE)

# Load FIPS codes
STATEFP <- 
    read.csv("data_raw/State FIPS codes.csv") %>%
    mutate(FIPS = sprintf("%02s", as.character(FIPS))) %>%
    dplyr::select(FIPS, state = stateabb)

non_CONUS_FIPS <- c("60", "66", "69", "72", "78", "02", "15")

fipsihme <- load(file="data/ihme_fips.rda")

# Load functions
source("code/00_functions.R")

# Plot preferences for the forest plot (from Jeff's 2022 SEDA paper)
DEFAULT_COLOR = "#737373"
LINE_COLOR <- "#528fad"
base_theme <- 
    theme_classic(base_size = 15) + 
    theme(
        panel.spacing = unit(1, "lines"),
        axis.title.x = element_text(color=DEFAULT_COLOR),
        axis.title.y = element_text(color=DEFAULT_COLOR),
        axis.line = element_line(color=DEFAULT_COLOR, size=0.25),
        axis.text.x = element_text(color=DEFAULT_COLOR), 
        axis.text.y = element_text(color=DEFAULT_COLOR),
        axis.ticks = element_line(color=DEFAULT_COLOR, size=0.25), 
        legend.text = element_text(color=DEFAULT_COLOR),
        legend.title = element_text(color=DEFAULT_COLOR),
        plot.subtitle = element_text(color=DEFAULT_COLOR),
        strip.background = element_rect(color=DEFAULT_COLOR, fill=NA, size=0.5),
        strip.text = element_text(color=DEFAULT_COLOR)
    )
