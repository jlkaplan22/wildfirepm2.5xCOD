#### Shortcut file to quickly prepare final analysis dataset for interim analyses ####
#### ALL MARITAL STATUSES and SEXES INCLUDED ####

rm(list = ls())
source("code/00_config.R")

PRISM <- read_csv(paste0(data_dir, "prism_monthly.csv"))

PRISM_popw <- read_csv(paste0(data_dir, "prism_monthly_popw.csv"))

county_smokePM_features <- readRDS("data/county_smokePM_features_2006_2020.rds")

#Load all-cause mortality data
COD <- 
    readRDS(paste0(raw_data_dir, "age_standardized_rates/age_standardized_rates-county_month-by_marital-all_cause.RDS")) %>% 
    mutate(statefips = str_sub(fipsihme, 1, 2)) %>% 
    filter(statefips %in% non_CONUS_FIPS == FALSE) %>%  
    filter(
        year >= 2009,
        year <= 2020,
        race_eth == "all",
        age_group == "15_and_up",
        death_type == "all_cause"
    )


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
    filter(yearmonth <= "2020-02-01", yearmonth >= "2009-01-01") #stop before COVID-19 pandemic

