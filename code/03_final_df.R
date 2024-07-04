#### Create final analysis df ####

# Load data
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

bothweights_county_smokePM_features <- readRDS("~/Documents/wildfirepm2.5xCOD/data/bothweights_county_smokePM_features_2006_2020.rds")
new_PRISM_areaw <- read_csv("data/new_PRISM_areaw.csv")
new_PRISM_popw <- read_csv("data/new_PRISM_popw.csv")

# Pull together final df
final <-
    COD %>% 
    left_join(
        new_PRISM_areaw,
        by=c("fipsihme" = "GEOID", "year", "month")
    ) %>% 
    left_join(
        new_PRISM_popw,
        by=c("fipsihme" = "GEOID", "year", "month")
    ) %>%
    left_join(
        bothweights_county_smokePM_features,
        by=c("fipsihme" = "GEOID", "year", "month")
    ) %>% 
    #replace NA values with zeroes--NAs occur when county has 0 smoke PM2.5 in a month
    mutate(
        
        # Calculate variables for area-weighted PM2.5
        areaw_mean_pm2.5 = ifelse(is.na(areaw_mean_pm2.5), 0, areaw_mean_pm2.5),
        areaw_cum_pm2.5 = ifelse(is.na(areaw_cum_pm2.5), 0, areaw_cum_pm2.5),
        areaw_daysover0 = ifelse(is.na(areaw_daysover0), 0, areaw_daysover0),
        areaw_daysover5 = ifelse(is.na(areaw_daysover5), 0, areaw_daysover5),
        areaw_daysover12.5 = ifelse(is.na(areaw_daysover12.5), 0, areaw_daysover12.5),
        areaw_daysover20 = ifelse(is.na(areaw_daysover20), 0, areaw_daysover20),
        areaw_daysover40 = ifelse(is.na(areaw_daysover40), 0, areaw_daysover40),
        
        # Calculate variables for population-weighted PM2.5
        popw_mean_pm2.5 = ifelse(is.na(popw_mean_pm2.5), 0, popw_mean_pm2.5),
        popw_cum_pm2.5 = ifelse(is.na(popw_cum_pm2.5), 0, popw_cum_pm2.5),
        popw_daysover0 = ifelse(is.na(popw_daysover0), 0, popw_daysover0),
        popw_daysover5 = ifelse(is.na(popw_daysover5), 0, popw_daysover5),
        popw_daysover12.5 = ifelse(is.na(popw_daysover12.5), 0, popw_daysover12.5),
        popw_daysover20 = ifelse(is.na(popw_daysover20), 0, popw_daysover20),
        popw_daysover40 = ifelse(is.na(popw_daysover40), 0, popw_daysover40),
        
        yearmonth = ym(paste(year, month, sep = "-"))
    ) %>% 
    filter(yearmonth <= "2020-02-01") #stop before COVID-19 pandemic

write_csv(final, paste0(data_dir, "final_7.01.24.csv"))

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
        new_PRISM_areaw, 
        by=c("fipsihme" = "GEOID", "year", "month")
    ) %>% 
    left_join(
        new_PRISM_popw,
        by=c("fipsihme" = "GEOID", "year", "month")
    ) %>% 
    left_join(
        bothweights_county_smokePM_features,
        by=c("fipsihme" = "GEOID", "year", "month")
    ) %>% 
    #replace NA values with zeroes--NAs occur due to ??
    mutate(
        
        # Calculate variables for area-weighted PM2.5
        areaw_mean_pm2.5 = ifelse(is.na(areaw_mean_pm2.5), 0, areaw_mean_pm2.5),
        areaw_cum_pm2.5 = ifelse(is.na(areaw_cum_pm2.5), 0, areaw_cum_pm2.5),
        areaw_daysover0 = ifelse(is.na(areaw_daysover0), 0, areaw_daysover0),
        areaw_daysover5 = ifelse(is.na(areaw_daysover5), 0, areaw_daysover5),
        areaw_daysover12.5 = ifelse(is.na(areaw_daysover12.5), 0, areaw_daysover12.5),
        areaw_daysover20 = ifelse(is.na(areaw_daysover20), 0, areaw_daysover20),
        areaw_daysover40 = ifelse(is.na(areaw_daysover40), 0, areaw_daysover40),
        
        # Calculate variables for population-weighted PM2.5
        popw_mean_pm2.5 = ifelse(is.na(popw_mean_pm2.5), 0, popw_mean_pm2.5),
        popw_cum_pm2.5 = ifelse(is.na(popw_cum_pm2.5), 0, popw_cum_pm2.5),
        popw_daysover0 = ifelse(is.na(popw_daysover0), 0, popw_daysover0),
        popw_daysover5 = ifelse(is.na(popw_daysover5), 0, popw_daysover5),
        popw_daysover12.5 = ifelse(is.na(popw_daysover12.5), 0, popw_daysover12.5),
        popw_daysover20 = ifelse(is.na(popw_daysover20), 0, popw_daysover20),
        popw_daysover40 = ifelse(is.na(popw_daysover40), 0, popw_daysover40),
        
        yearmonth = ym(paste(year, month, sep = "-"))
    ) %>% 
    filter(yearmonth <= "2020-02-01") #stop before COVID-19 pandemic









