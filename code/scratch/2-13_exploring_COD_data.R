### 2-13-23 first access to COD data, doing some exploration ####

# load COD data--takes a while (a bit over 2 min)
allCOD <- readRDS("data_raw/age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-all_cause.RDS")
cancer <- readRDS("data_raw/age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-cancer.RDS")
cvd <- readRDS("data_raw/age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-cardiovascular.RDS")
parasitic <- readRDS("data_raw/age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-infectious_parasitic.RDS")
injuries <- readRDS("data_raw/age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-injuries.RDS")
neuropsych <- readRDS("data_raw/age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-neuropsychiatric.RDS")
resp <- readRDS("data_raw/age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-respiratory.RDS")

# pull county FIPS codes and set to ihme stable codes
fipsihme_sf <- 
    counties(year = 2020) %>% # year should be 2006 once those data are working again
    filter(STATEFP %in% non_CONUS_FIPS == F) %>% 
    dplyr::mutate(COUNTYFP = paste0(STATEFP, COUNTYFP)) %>% 
    dplyr::select(COUNTYFP) %>% 
    left_join(
        ihme_fips %>% dplyr::select(COUNTYFP = orig_fips, ihme_fips),
        by=c("COUNTYFP")
    ) %>% 
    mutate(
        ihme_fips = ifelse(is.na(ihme_fips), COUNTYFP, ihme_fips)
    ) %>% 
    dplyr::select(fipsihme = ihme_fips, COUNTYFP)

# group by county-month, collapse all race/ethnicity categories
overall_COD <-
    allCOD %>% 
    filter(year >= 2006 & race_eth == "all" & age_group == "all_ages") %>% 
    group_by(fipsihme, year, month) %>% 
    summarise(pop = sum(pop), n_deaths = sum(n_deaths), rate = n_deaths / pop)

# total county-months by year of data, and total deaths per year--12 counties drop
# off after 2006-2009
overall_COD %>% 
    group_by(year) %>% 
    summarise(n = n(), n_deaths = sum(n_deaths))

# quick map of mortality rate by county, for 2010 (random year)
# mortality2010_sf <-
#     overall_COD %>% 
#     group_by(year, fipsihme) %>% 
#     summarise(rate = sum(n_deaths) / sum(pop)) %>% 
#     filter(year == 2010) %>% 
#     left_join(fipsihme_sf)
# 
# 
# tm_shape(mortality2010_sf) +
#     tm_fill(col = "rate")

# some basic exploratory graphs
allCOD %>% 
    ggplot(aes(pop)) +
    geom_histogram() +
    scale_y_log10()

allCOD %>% 
    ggplot(aes(n_deaths)) +
    geom_histogram() +
    scale_y_log10()

allCOD %>% 
    filter(rate != 0) %>% 
    ggplot(aes(rate)) +
    geom_boxplot() 

suspicious <-
    allCOD %>% 
    filter(n_deaths >= pop*.5)

# look at TWFE variance analysis like Burke suggested
COD_var <-
    overall_COD %>%
    group_by(year, month) %>%
    mutate(
        month_mean_deathrate = mean(rate),
        time_FE = rate - month_mean_deathrate, 
    ) %>%
    ungroup() %>%
    group_by(fipsihme) %>%
    mutate(
        mean_county_deathrate = mean(rate),
        district_FE = rate - mean_county_deathrate
    ) %>%
    ungroup() %>%
    mutate(TWFE = rate - time_FE - district_FE) %>%
    summarise(
        raw_mean = mean(rate),
        raw_stdev = sd(rate),
        raw_var = var(rate),
        district_FE_var = var(district_FE),
        time_FE_var = var(time_FE),
        TWFE_var = var(TWFE)
    )

COD_var$TWFE_var / COD_var$raw_var #.63, so the FE aren't absorbing too much variation

# draft putting together final analysis dataset, just to get a sense of what it would look like:
mock_final <-
    overall_COD %>% 
    filter(year >= 2009) %>% 
    left_join(
        acs2009to2020,
        by=c("fipsihme" = "GEOID", "year")
    ) %>% 
    left_join(
        PRISM, 
        by=c("fipsihme" = "GEOID", "year", "month")
    )

names(mock_final)

mock_final %>% 
    ggplot(aes(x = pop.x, y = pop.y)) +
    geom_point(size=.1) 

mock_final %>% 
    mutate(statefips = str_sub(fipsihme, 1, 2)) %>% 
    ggplot(aes(x = pop.x, y = pop.y)) +
    geom_point(size=.1) +
    facet_wrap(facets = vars(statefips), scales="free")







