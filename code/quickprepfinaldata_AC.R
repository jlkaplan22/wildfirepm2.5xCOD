#### Shortcut file to quickly prepare final analysis dataset for interim analyses ####
rm(list = ls())
source("code/00_config.R")

PRISM <- read_csv(paste0(data_dir, "prism_monthly.csv"))

PRISM_popw <- read_csv(paste0(data_dir, "prism_monthly_popw.csv"))

avg_county_smokePM <- readRDS("data/county_smokePM_predictions_20060101_20201231.rds")

# aggregate to monthly level and create thresholding features
county_smokePM_features <-
    avg_county_smokePM %>% 
    mutate(year = year(date), month = month(date)) %>% 
    group_by(GEOID, year, month) %>% 
    summarise(
        cum_pm2.5 = sum(smokePM_pred),
        mean_pm2.5 =
            case_when(
                month %in% c(1, 3, 5, 7, 8, 10, 12) ~ cum_pm2.5 / 31,
                month %in% c(4, 6, 9, 11) ~ cum_pm2.5 / 30,
                month %in% c(2) ~ cum_pm2.5 / 28
            ),
        daysover0 = sum(smokePM_pred > 0),
        daysover5 = sum(smokePM_pred > 5),
        daysover12.5 = sum(smokePM_pred > 12.5),
        daysover20 = sum(smokePM_pred > 20),
        daysover40 = sum(smokePM_pred > 40)
    ) %>% 
    distinct()

#Load all-cause mortality data
COD <- 
    #readRDS(paste0(raw_data_dir, "age_standardized_rates/age_standardized_rates-county_month-by_marital-all_cause.RDS")) %>% 
    readRDS(paste0(raw_data_dir, "age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-all_cause.RDS")) %>% 
    mutate(statefips = str_sub(fipsihme, 1, 2)) %>% 
    filter(statefips %in% non_CONUS_FIPS == FALSE) %>%  
    filter(
        year >= 2006,
        year <= 2020,
        race_eth == "all",
        #age_group == "15_and_up",
        age_group == "all_ages",
        marital == "all", 
        sex == "all",
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
    #filter(yearmonth <= "2020-02-01", yearmonth >= "2009-01-01") #stop before COVID-19 pandemic
    filter(yearmonth <= "2020-02-01") #stop before COVID-19 pandemic


