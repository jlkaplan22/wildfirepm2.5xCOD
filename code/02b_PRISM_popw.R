#### Load and clean PRISM data with population weighting ####
# Process is similar to `01_county_pm2.5.R`

### Daily data, run using function by year, leveraging exisiting downloads:

get_PRISM_popw_daily_temps <- function(year) {
    # Set directory to download data to--can delete this after running the function over a year,
    #   since it will be a large amount of data (~1 GB) per year
    options(prism.path = paste0(raw_data_dir, "prism/temp_dailys/", "temp", year))
    
    temp_stack <- pd_stack(prism_archive_ls())
    
    # load county sf
    counties_sf <- 
        counties %>% 
        filter(STATEFP %in% non_CONUS_FIPS == F) %>% 
        dplyr::select(GEOID)
    
    # read in the grid
    grid_10km <- 
        st_read(paste0(raw_data_dir, "10km_grid/10km_grid_wgs84/10km_grid_wgs84.shp")) %>% 
        st_transform(st_crs(counties_sf))
    
    # extract daily PRISM metrics to 10km grid
    temp_extract_10km <- 
        cbind(
            grid_10km$ID,
            exact_extract(terra::rast(temp_stack), grid_10km, fun="mean", progress=T)
        ) %>% 
        rename(ID = `grid_10km$ID`)
    
    # merge in population data
    pop <-
        list.files("data_raw/populationDensity_10km_subgrid", full.names = TRUE) %>% 
        purrr::map_dfr(read_csv)
    
    # load grid-to-county xwalk from `01_county_pm2.5.R`
    county_cross <- readRDS("data/county_xwalk")
    
    # tidy data
    tidy_temp_10km <-
        temp_extract_10km %>% 
        st_drop_geometry() %>% 
        pivot_longer(cols = -c(ID), names_to = "date", values_to = "temp") %>% 
        mutate(
            year = str_sub(date, 31, 34),
            month = str_sub(date, 35, 36),
            day = str_sub(date, 37, 38)
        ) %>% 
        dplyr::select(-date)
    
    # aggregate to county level using population-weighting
    temp_county <-
        tidy_temp_10km %>% 
        left_join(
            pop %>% dplyr::select(ID, grid_pop_per_m2 = mean),
            by=c("ID")
        ) %>% 
        left_join(
            county_cross,
            by=c("ID" = "grid_id_10km")
        ) %>% 
        mutate(area2 = unclass(area), 
               pop = grid_pop_per_m2*area2) %>%
        group_by(GEOID, year, month, day) %>% 
        summarise(weighted_temp = weighted.mean(temp, pop)) %>% 
        ungroup()

    write_csv(temp_county, paste0(data_dir, "PRISM", "/PRISM_popw_daily_temp_", year, ".csv"))
}

for (year in 2019:2020) {
    get_PRISM_popw_daily_temps(year)
}

get_PRISM_popw_daily_precips <- function(year) {
    # Set directory to download data to--can delete this after running the function over a year,
    #   since it will be a large amount of data (~1 GB) per year
    options(prism.path = paste0(raw_data_dir, "prism/precip_dailys/", "precip", year))
    
    precip_stack <- pd_stack(prism_archive_ls())
    
    # load county sf
    counties_sf <- 
        counties %>% 
        filter(STATEFP %in% non_CONUS_FIPS == F) %>% 
        dplyr::select(GEOID)
    
    # read in the grid
    grid_10km <- 
        st_read(paste0(raw_data_dir, "10km_grid/10km_grid_wgs84/10km_grid_wgs84.shp")) %>% 
        st_transform(st_crs(counties_sf))
    
    # extract daily PRISM metrics to 10km grid
    precip_extract_10km <- 
        cbind(
            grid_10km$ID,
            exact_extract(terra::rast(precip_stack), grid_10km, fun="mean", progress=T)
        ) %>% 
        rename(ID = `grid_10km$ID`)
    
    # merge in population data
    pop <-
        list.files("data_raw/populationDensity_10km_subgrid", full.names = TRUE) %>% 
        purrr::map_dfr(read_csv)
    
    # load grid-to-county xwalk from `01_county_pm2.5.R`
    county_cross <- readRDS("data/county_xwalk")
    
    # tidy data
    tidy_precip_10km <-
        precip_extract_10km %>% 
        st_drop_geometry() %>% 
        pivot_longer(cols = -c(ID), names_to = "date", values_to = "ppt") %>% 
        mutate(
            year = str_sub(date, 29, 32),
            month = str_sub(date, 33, 34),
            day = str_sub(date, 35, 36)
        ) %>% 
        dplyr::select(-date)
    
    # aggregate to county level using population-weighting
    precip_county <-
        tidy_precip_10km %>% 
        left_join(
            pop %>% dplyr::select(ID, grid_pop_per_m2 = mean),
            by=c("ID")
        ) %>% 
        left_join(
            county_cross,
            by=c("ID" = "grid_id_10km")
        ) %>% 
        mutate(area2 = unclass(area), 
               pop = grid_pop_per_m2*area2) %>%
        group_by(GEOID, year, month, day) %>% 
        summarise(weighted_precip = weighted.mean(ppt, pop)) %>% 
        ungroup()
    
    write_csv(precip_county, paste0(data_dir, "PRISM", "/PRISM_popw_daily_precip_", year, ".csv"))
}

for (year in 2006:2020) {
    get_PRISM_popw_daily_precips(year)
}

## Aggregate year files and create different PRISM constructs
daily_temps <- data.frame()
for (year in 2006:2020) {
    temp_data <- read.csv(paste0(data_dir, "PRISM", "/PRISM_popw_daily_temp_", year, ".csv"))
    daily_temps <- rbind(daily_temps, temp_data)
}

daily_precips <- data.frame()
for (year in 2006:2020) {
    precip_data <- read.csv(paste0(data_dir, "PRISM", "/PRISM_popw_daily_precip_", year, ".csv"))
    daily_precips <- rbind(daily_precips, precip_data)
}

aggregated_temp <-
    daily_temps %>% 
    group_by(GEOID, year, month) %>% 
    summarize(
        popw_monthly_mean_tempC = mean(weighted_temp),
        popw_monthly_mean_tempF = mean(celsius_to_fahrenheit(weighted_temp)),

        #Binned analysis
        popw_days_u30 = sum(celsius_to_fahrenheit(weighted_temp) < 30),
        popw_days_30to40 = sum(celsius_to_fahrenheit(weighted_temp) >= 30 & celsius_to_fahrenheit(weighted_temp) < 40),
        popw_days_40to50 = sum(celsius_to_fahrenheit(weighted_temp) >= 40 & celsius_to_fahrenheit(weighted_temp) < 50),
        popw_days_50to60 = sum(celsius_to_fahrenheit(weighted_temp) >= 50 & celsius_to_fahrenheit(weighted_temp) < 60),
        popw_days_60to70 = sum(celsius_to_fahrenheit(weighted_temp) >= 60 & celsius_to_fahrenheit(weighted_temp) < 70),
        popw_days_70to80 = sum(celsius_to_fahrenheit(weighted_temp) >= 70 & celsius_to_fahrenheit(weighted_temp) < 80),
        popw_days_80to90 = sum(celsius_to_fahrenheit(weighted_temp) >= 80 & celsius_to_fahrenheit(weighted_temp) < 90),
        popw_days_90to100 = sum(celsius_to_fahrenheit(weighted_temp) >= 90 & celsius_to_fahrenheit(weighted_temp) < 100),
        popw_days_o100 = sum(celsius_to_fahrenheit(weighted_temp) > 100)
    )

aggregated_precip <-
    daily_precips %>% 
    group_by(GEOID, year, month) %>% 
    summarize(
        popw_monthly_mean_precip = mean(weighted_precip),
        popw_days_w_precip = sum(weighted_precip > 0),
        popw_days_above_5mm = sum(weighted_precip > 5),
        popw_days_above_10mm = sum(weighted_precip > 10),
        popw_days_above_15mm = sum(weighted_precip > 15),
        popw_days_above_30mm = sum(weighted_precip > 30),
        popw_total_precip = sum(weighted_precip)
    )

new_PRISM_popw <-
    full_join(
        aggregated_temp, 
        aggregated_precip, 
        by=c("GEOID", "year", "month")
    ) %>% 
    mutate(
        GEOID = GEOID %>% as.character(),
        GEOID =
            ifelse(
                str_length(GEOID) == 4, 
                paste0("0", GEOID),
                GEOID
            )
    )

write_csv(new_PRISM_popw, paste0(data_dir, "new_PRISM_popw.csv"))

#compare results to area-weighted results, should be mostly similar but not exactly colinear
compare <-
    full_join(
        new_PRISM_popw,
        new_PRISM
    )

compare %>% 
    ggplot(aes(x=monthly_mean_tempF, y=popw_monthly_mean_tempF)) +
    geom_point(alpha=.1) +
    geom_abline(slope = 1, linewidth = .1) 

compare %>% 
    ggplot(aes(x=days_above_90F, y=popw_days_above_90F)) +
    geom_point(alpha=.1) +
    geom_abline(slope = 1, linewidth = .1) 

compare %>% 
    ggplot(aes(x=monthly_mean_precip, y=popw_monthly_mean_precip)) +
    geom_point(alpha=.1) +
    geom_abline(slope = 1, linewidth = .1) 

