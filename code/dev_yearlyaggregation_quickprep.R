#### Shortcut file to quickly prepare final analysis dataset annually-aggregated analysis ####
rm(list = ls())
source("code/00_config.R")

PRISM <- 
    read_csv(paste0(data_dir, "prism_monthly.csv")) %>% 
    mutate(
        season = 
            case_when(
                month == 6 | month == 7 | month == 8 ~ "summer",
                month == 1 | month == 2 | month == 3 ~ "winter",
                TRUE ~ "other"
            )
    ) %>% 
    group_by(GEOID, year, season) %>% 
    summarise(
        temp = mean(temp),
        precip = mean(precip)
    ) %>% 
    filter(season != "other") %>% 
    pivot_wider(names_from=season, values_from=temp:precip)

PRISM_popw <- 
    read_csv(paste0(data_dir, "prism_monthly_popw.csv")) %>% 
    mutate(
        season = 
            case_when(
                month == 6 | month == 7 | month == 8 ~ "summer",
                month == 1 | month == 2 | month == 3 ~ "winter",
                TRUE ~ "other"
            )
    ) %>% 
    group_by(GEOID, year, season) %>% 
    summarise(
        weighted_temp = mean(weighted_temp),
        weighted_precip = mean(weighted_precip)
    ) %>% 
    filter(season != "other") %>% 
    pivot_wider(names_from=season, values_from=weighted_temp:weighted_precip)

avg_county_smokePM <- readRDS("data/county_smokePM_predictions_20060101_20201231.rds")

# aggregate to monthly level and create thresholding features
county_smokePM_features <-
    avg_county_smokePM %>% 
    mutate(year = year(date), month = month(date)) %>% 
    group_by(GEOID, year) %>% 
    summarise(
        cum_pm2.5 = sum(smokePM_pred),
        mean_pm2.5 = cum_pm2.5 / 365,
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
    ) %>% 
    group_by(fipsihme, year) %>% 
    summarise(n_deaths = sum(n_deaths), pop = mean(pop))


final <-
    COD %>% 
    left_join(
        PRISM, 
        by=c("fipsihme" = "GEOID", "year")
    ) %>% 
    left_join(
        PRISM_popw,
        by=c("fipsihme" = "GEOID", "year")
    ) %>% 
    left_join(
        county_smokePM_features,
        by=c("fipsihme" = "GEOID", "year")
    ) %>% 
    mutate(
        mean_pm2.5 = ifelse(is.na(mean_pm2.5), 0, mean_pm2.5),
        cum_pm2.5 = ifelse(is.na(cum_pm2.5), 0, cum_pm2.5),
        daysover0 = ifelse(is.na(daysover0), 0, daysover0),
        daysover5 = ifelse(is.na(daysover5), 0, daysover5),
        daysover12.5 = ifelse(is.na(daysover12.5), 0, daysover12.5),
        daysover20 = ifelse(is.na(daysover20), 0, daysover20),
        daysover40 = ifelse(is.na(daysover40), 0, daysover40)
    ) %>% 
    filter(year < "2020") #stop before COVID-19 pandemic


