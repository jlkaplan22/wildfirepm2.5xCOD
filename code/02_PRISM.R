#### Load and clean PRISM data with area weighting ####

# download PRISM temp data
options(prism.path=paste0(raw_data_dir, "prism/temp"))
get_prism_monthlys(type="tmean", years = 2006:2020, mon = seq(1:12), keepZip=FALSE)
temp_stack <- pd_stack(prism_archive_ls())

options(prism.path=paste0(raw_data_dir, "prism/precip"))
get_prism_monthlys(type="ppt", years = 2006:2020, mon = seq(1:12), keepZip=FALSE)
precip_stack <- pd_stack(prism_archive_ls())

# load county sf
counties_sf <- 
    counties(year = 2020) %>% # year should be 2006 once those data are working again
    filter(STATEFP %in% non_CONUS_FIPS == F) %>% 
    dplyr::select(GEOID)

# extract PRISM metrics
temp_extract <- 
    cbind(
        counties_sf,
        exact_extract(terra::rast(temp_stack), counties_sf, fun="mean", progress=T)
    )
    
precip_extract <- 
    cbind(
        counties_sf,
        exact_extract(terra::rast(precip_stack), counties_sf, fun="mean", progress=T)
    )
    

# tidy data
temp <-
    temp_extract %>% 
    st_drop_geometry() %>% 
    pivot_longer(cols = -c(GEOID), names_to = "date", values_to = "temp") %>% 
    mutate(
        year = str_sub(date, 31, 34),
        month = str_sub(date, 35, 36)
    ) %>% 
    dplyr::select(-date)

precip <-
    precip_extract %>% 
    st_drop_geometry() %>% 
    pivot_longer(cols = -c(GEOID), names_to = "date", values_to = "precip") %>% 
    mutate(
        year = str_sub(date, 29, 32),
        month = str_sub(date, 33, 34)
    ) %>% 
    dplyr::select(-date)

PRISM <-
    full_join(
        temp,
        precip
    ) %>% 
    dplyr::select(GEOID, year, month, temp, precip) %>% 
    mutate(month = as.numeric(month))

write_csv(PRISM, paste0(data_dir, "prism_monthly.csv"))

# shortcut for once this file has been run initially
#PRISM <- read_csv(paste0(data_dir, "prism_monthly.csv"))


