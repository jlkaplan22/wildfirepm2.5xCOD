#### Load and tidy COD data ####

#Load all-cause mortality data with marital status
COD_ms <-
    readRDS(paste0(raw_data_dir, "age_standardized_rates/age_standardized_rates-county_month-by_marital-all_cause.RDS")) %>% 
    mutate(statefips = str_sub(fipsihme, 1, 2)) %>% 
    filter(statefips %in% non_CONUS_FIPS == FALSE) %>%  
    filter(
        year >= 2009,
        year <= 2020,
        race_eth == "all",
        death_type == "all_cause"
    )

# code block that's useful for figuring out what the different breakdowns are
readRDS(paste0(raw_data_dir, "age_standardized_rates/age_standardized_rates-county_month-by_marital-all_cause.RDS")) %>%
    dplyr::select(death_type, race_eth, marital, sex, age_group) %>%
    distinct() %>%
    print(n = 45)

#Load all-cause mortality data, with all subgroups combined
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


# Ignore other groups for now--just focus on all-cause mortality
cancer <- 
    readRDS(paste0(raw_data_dir, "age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-cancer.RDS")) %>% 
    mutate(statefips = str_sub(fipsihme, 1, 2)) %>% 
    filter(statefips %in% non_CONUS_FIPS == FALSE) %>%  
    filter(
        year >= 2006, #replicating Ma et al as of 2/23/23
        year <= 2016,
        race_eth == "all",
        age_group == "all_ages",
        sex == "all",
        marital == "all",
        death_type == "cancer")

cvd <- 
    readRDS(paste0(raw_data_dir, "age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-cardiovascular.RDS")) %>% 
    mutate(statefips = str_sub(fipsihme, 1, 2)) %>% 
    filter(statefips %in% non_CONUS_FIPS == FALSE) %>%  
    filter(
        year >= 2006, #replicating Ma et al as of 2/23/23
        year <= 2016,
        race_eth == "all",
        age_group == "all_ages",
        sex == "all",
        marital == "all",
        death_type == "cardiovascular"
    )

infectious_parasitic <- 
    readRDS(paste0(raw_data_dir, "age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-infectious_parasitic.RDS")) %>% 
    mutate(statefips = str_sub(fipsihme, 1, 2)) %>% 
    filter(statefips %in% non_CONUS_FIPS == FALSE) %>%  
    filter(
        year >= 2006, #replicating Ma et al as of 2/23/23
        year <= 2016,
        race_eth == "all",
        age_group == "all_ages",
        sex == "all",
        marital == "all",
        death_type == "infectious_parasitic"
    )
injuries <- 
    readRDS(paste0(raw_data_dir, "age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-injuries.RDS")) %>% 
    mutate(statefips = str_sub(fipsihme, 1, 2)) %>% 
    filter(statefips %in% non_CONUS_FIPS == FALSE) %>%  
    filter(
        year >= 2006, #replicating Ma et al as of 2/23/23
        year <= 2016,
        race_eth == "all",
        age_group == "all_ages",
        sex == "all",
        marital == "all",
        death_type == "injuries"
    )
neuropsych <- 
    readRDS(paste0(raw_data_dir, "age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-neuropsychiatric.RDS")) %>% 
    mutate(statefips = str_sub(fipsihme, 1, 2)) %>% 
    filter(statefips %in% non_CONUS_FIPS == FALSE) %>%  
    filter(
        year >= 2006, #replicating Ma et al as of 2/23/23
        year <= 2016,
        race_eth == "all",
        age_group == "all_ages",
        sex == "all",
        marital == "all",
        death_type == "neuropsychiatric"
    )
resp <- 
    readRDS(paste0(raw_data_dir, "age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-respiratory.RDS")) %>% 
    mutate(statefips = str_sub(fipsihme, 1, 2)) %>% 
    filter(statefips %in% non_CONUS_FIPS == FALSE) %>%  
    filter(
        year >= 2006, #replicating Ma et al as of 2/23/23
        year <= 2016,
        race_eth == "all",
        age_group == "all_ages",
        sex == "all",
        marital == "all",
        death_type == "respiratory"
    )


