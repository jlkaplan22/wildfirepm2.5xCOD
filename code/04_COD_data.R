#### Load and tidy COD data ####

#Load all-cause mortality data, with all subgroups combined
COD <- 
    readRDS(paste0(raw_data_dir, "age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-all_cause.RDS")) %>% 
    mutate(statefips = str_sub(fipsihme, 1, 2)) %>% 
    filter(statefips %in% non_CONUS_FIPS == FALSE) %>%  
    filter(
        year >= 2006, #replicating Ma et al as of 2/23/23
        year <= 2016,
        race_eth == "all",
        age_group == "all_ages",
        sex == "all",
        marital == "all",
        death_type == "all_cause"
    )


# Ignore other groups for now--just focus on all-cause mortality
cancer <- 
    readRDS(paste0(raw_data_dir, "age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-cancer.RDS"))
cvd <- 
    readRDS(paste0(raw_data_dir, "age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-cardiovascular.RDS"))
parasitic <- 
    readRDS(paste0(raw_data_dir, "age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-infectious_parasitic.RDS"))
injuries <- 
    readRDS(paste0(raw_data_dir, "age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-injuries.RDS"))
neuropsych <- 
    readRDS(paste0(raw_data_dir, "age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-neuropsychiatric.RDS"))
resp <- 
    readRDS(paste0(raw_data_dir, "age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-respiratory.RDS"))

