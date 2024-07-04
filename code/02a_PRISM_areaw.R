#### Load and clean PRISM data with area weighting ####
# Creates functions to download daily PRISM data, then organizes data into 
# .csv format for each year so PRISM downloads can be deleted (~1GB per year, so lots of storage).
# Each .csv is ~30MB. Then uses .csv's to create aggregate PRISM variables of interest.

## download daily PRISM data
get_PRISM_daily_temps <- function(year) {
    # Set directory to download data to--can delete this after running the function over a year,
    #   since it will be a large amount of data (~1 GB) per year
    dir.create(paste0(raw_data_dir, "prism/temp_dailys/", "temp", year))
    options(prism.path = paste0(raw_data_dir, "prism/temp_dailys/", "temp", year))
    
    get_prism_dailys(type="tmean", 
                     minDate = paste0(year, "-01-01"), 
                     maxDate = paste0(year, "-12-31"), 
                     keepZip=FALSE)
    
    temp_stack <- pd_stack(prism_archive_ls())
    
    # load county sf
    counties_sf <- 
        counties %>% 
        filter(STATEFP %in% non_CONUS_FIPS == F) %>% 
        dplyr::select(GEOID)
    
    # extract daily PRISM metrics
    temp_daily_extract <- 
        cbind(
            counties_sf,
            exact_extract(terra::rast(temp_stack), counties_sf, fun="mean", progress=T)
        )
    
    # tidy data
    temp_daily <-
        temp_daily_extract %>% 
        st_drop_geometry() %>% 
        pivot_longer(cols = -c(GEOID), names_to = "date", values_to = "temp") %>% 
        mutate(
            year = str_sub(date, 31, 34),
            month = str_sub(date, 35, 36),
            day = str_sub(date, 37, 38)
        ) %>% 
        dplyr::select(-date)
    
    write_csv(temp_daily, paste0(data_dir, "PRISM", "/PRISM_daily_temp_", year, ".csv"))
}

for (year in 2019:2020) {
    get_PRISM_daily_temps(year)
}



get_PRISM_daily_precips <- function(year) {
    # Set directory to download data to--can delete this after running the function over a year,
    #   since it will be a large amount of data (~1 GB) per year
    
    dir.create(paste0(raw_data_dir, "prism/precip_dailys/", "precip", year))
    options(prism.path = paste0(raw_data_dir, "prism/precip_dailys/", "precip", year))
    
    get_prism_dailys(type="ppt",
                     minDate = paste0(year, "-01-01"),
                     maxDate = paste0(year, "-12-31"),
                     keepZip=FALSE)
    
    precip_stack <- pd_stack(prism_archive_ls())
    
    # load county sf
    counties_sf <- 
        counties %>% 
        filter(STATEFP %in% non_CONUS_FIPS == F) %>% 
        dplyr::select(GEOID)
    
    # extract daily PRISM metrics
    precip_daily_extract <- 
        cbind(
            counties_sf,
            exact_extract(terra::rast(precip_stack), counties_sf, fun="mean", progress=T)
        )
    
    # tidy data
    precip_daily <-
        precip_daily_extract %>% 
        st_drop_geometry() %>% 
        pivot_longer(cols = -c(GEOID), names_to = "date", values_to = "ppt") %>% 
        mutate(
            year = str_sub(date, 29, 32),
            month = str_sub(date, 33, 34),
            day = str_sub(date, 35, 36)
        ) %>% 
        dplyr::select(-date)
    
    write_csv(precip_daily, paste0(data_dir, "PRISM", "/PRISM_daily_precip_", year, ".csv"))
}

for (year in 2017:2020) {
    get_PRISM_daily_precips(year)
}

## Aggregate year files and create different PRISM constructs
daily_temps <- data.frame()
for (year in 2006:2020) {
    temp_data <- read.csv(paste0(data_dir, "PRISM", "/PRISM_daily_temp_", year, ".csv"))
    daily_temps <- rbind(daily_temps, temp_data)
}

daily_precips <- data.frame()
for (year in 2006:2020) {
    precip_data <- read.csv(paste0(data_dir, "PRISM", "/PRISM_daily_precip_", year, ".csv"))
    daily_precips <- rbind(daily_precips, precip_data)
}

aggregated_temp <-
    daily_temps %>% 
    group_by(GEOID, year, month) %>% 
    summarize(
        areaw_monthly_mean_tempC = mean(temp),
        areaw_monthly_mean_tempF = mean(celsius_to_fahrenheit(temp)),
        
        #Binned analysis
        areaw_days_u30 = sum(celsius_to_fahrenheit(temp) < 30),
        areaw_days_30to40 = sum(celsius_to_fahrenheit(temp) >= 30 & celsius_to_fahrenheit(temp) < 40),
        areaw_days_40to50 = sum(celsius_to_fahrenheit(temp) >= 40 & celsius_to_fahrenheit(temp) < 50),
        areaw_days_50to60 = sum(celsius_to_fahrenheit(temp) >= 50 & celsius_to_fahrenheit(temp) < 60),
        areaw_days_60to70 = sum(celsius_to_fahrenheit(temp) >= 60 & celsius_to_fahrenheit(temp) < 70),
        areaw_days_70to80 = sum(celsius_to_fahrenheit(temp) >= 70 & celsius_to_fahrenheit(temp) < 80),
        areaw_days_80to90 = sum(celsius_to_fahrenheit(temp) >= 80 & celsius_to_fahrenheit(temp) < 90),
        areaw_days_90to100 = sum(celsius_to_fahrenheit(temp) >= 90 & celsius_to_fahrenheit(temp) < 100),
        areaw_days_o100 = sum(celsius_to_fahrenheit(temp) > 100)
    )

aggregated_precip <-
    daily_precips %>% 
    group_by(GEOID, year, month) %>% 
    summarize(
        areaw_monthly_mean_precip = mean(ppt),
        areaw_days_w_precip = sum(ppt > 0),
        areaw_days_above_5mm = sum(ppt > 5),
        areaw_days_above_10mm = sum(ppt > 10),
        areaw_days_above_15mm = sum(ppt > 15),
        areaw_days_above_30mm = sum(ppt > 30),
        areaw_total_precip = sum(ppt)
    )

new_PRISM_areaw <-
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

write_csv(new_PRISM_areaw, paste0(data_dir, "new_PRISM_areaw.csv"))
