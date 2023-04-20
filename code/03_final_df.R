#### Load COD data and create final analysis df ####

# Load all-cause mortality data, with all subgroups combined
COD <- 
    readRDS(paste0(raw_data_dir, "age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-all_cause.RDS")) %>% 
    mutate(statefips = str_sub(fipsihme, 1, 2)) %>% 
    filter(statefips %in% non_CONUS_FIPS == FALSE) %>%  
    filter(
        year >= 2006,
        year <= 2020,
        race_eth == "all",
        age_group == "all_ages",
        sex == "all",
        marital == "all",
        death_type == "all_cause"
    )

# Pull together final df
final <-
    COD %>% 
    left_join(
        PRISM, 
        by=c("fipsihme" = "GEOID", "year", "month")
    ) %>% 
    left_join(
        PRISM_popw,
        by=c("fipsihme" = "GEOID", "year", "month")
    ) %>% 
    left_join(
        county_smokePM_features,
        by=c("fipsihme" = "GEOID", "year", "month")
    ) %>% 
    #replace NA values with zeroes--NAs occur when county has 0 smoke PM2.5 in a month
    mutate(
        mean_pm2.5 = ifelse(is.na(mean_pm2.5), 0, mean_pm2.5),
        cum_pm2.5 = ifelse(is.na(cum_pm2.5), 0, cum_pm2.5),
        daysover0 = ifelse(is.na(daysover0), 0, daysover0),
        daysover5 = ifelse(is.na(daysover5), 0, daysover5),
        daysover12.5 = ifelse(is.na(daysover12.5), 0, daysover12.5),
        daysover20 = ifelse(is.na(daysover20), 0, daysover20),
        daysover40 = ifelse(is.na(daysover40), 0, daysover40),
        
        yearmonth = ym(paste(year, month, sep = "-"))
    ) %>% 
    filter(yearmonth <= "2020-02-01") #stop before COVID-19 pandemic

# Load all-cause mortality data, stratified by age
COD_agestratified <- 
    readRDS(paste0(raw_data_dir, "age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-all_cause.RDS")) %>% 
    mutate(statefips = str_sub(fipsihme, 1, 2)) %>% 
    filter(statefips %in% non_CONUS_FIPS == FALSE) %>%  
    filter(
        year >= 2006,
        year <= 2020,
        race_eth == "all",
        age_group == "65_and_up" | age_group == "under_65",
        marital == "all", 
        sex == "all",
        death_type == "all_cause"
    )

# Final df
final_agestratified <-
    COD_agestratified %>% 
    left_join(
        PRISM, 
        by=c("fipsihme" = "GEOID", "year", "month")
    ) %>% 
    left_join(
        PRISM_popw,
        by=c("fipsihme" = "GEOID", "year", "month")
    ) %>% 
    left_join(
        county_smokePM_features,
        by=c("fipsihme" = "GEOID", "year", "month")
    ) %>% 
    #replace NA values with zeroes--NAs occur due to ??
    mutate(
        mean_pm2.5 = ifelse(is.na(mean_pm2.5), 0, mean_pm2.5),
        cum_pm2.5 = ifelse(is.na(cum_pm2.5), 0, cum_pm2.5),
        daysover0 = ifelse(is.na(daysover0), 0, daysover0),
        daysover5 = ifelse(is.na(daysover5), 0, daysover5),
        daysover12.5 = ifelse(is.na(daysover12.5), 0, daysover12.5),
        daysover20 = ifelse(is.na(daysover20), 0, daysover20),
        daysover40 = ifelse(is.na(daysover40), 0, daysover40),
        
        yearmonth = ym(paste(year, month, sep = "-"))
    ) %>% 
    filter(yearmonth <= "2020-02-01") #stop before COVID-19 pandemic
