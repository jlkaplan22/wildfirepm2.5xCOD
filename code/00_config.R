# Load libraries
library(tidyverse)
library(ggplot2)
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


library(INLA) #likely won't actually be used since this is for Bayesian models from Chen et al.
# following lines needed to install INLA for newer versions of R:
# install.packages("INLA",
#                  repos = "https://inla.r-inla-download.org/R/stable", dep = TRUE)

library(pglm)
library(poisFErobust) #testing this out, mayor may not end up using it

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
census_api_key("2828600b30d7cbcaf6b2375c40e137c3bef459c7", install=TRUE, overwrite=TRUE)

# Load FIPS codes
STATEFP <- 
    read.csv("data_raw/State FIPS codes.csv") %>%
    mutate(FIPS = sprintf("%02s", as.character(FIPS))) %>%
    dplyr::select(FIPS, state = stateabb)

non_CONUS_FIPS <- c("60", "66", "69", "72", "78", "02", "15")

fipsihme <- load(file="data/ihme_fips.rda")

# Load functions
source("code/00_functions.R")

