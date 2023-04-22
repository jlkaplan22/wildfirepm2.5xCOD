#### Generate descriptive table 2 ####

# Table 2: across-county distribution of smoke PM2.5 and mortality
cross_county_annual <-
    final %>% 
    filter(
        sex == "all",
        age_group == "all_ages",
        race_eth == "all",
        marital == "all",
    ) %>% 
    group_by(fipsihme) %>% 
    summarise(
        mean_pop = mean(pop),
        mean_monthly_deaths = mean(n_deaths),
        mean_deathrate = mean(rate),
        mean_pm2.5 = mean(mean_pm2.5),
        mean_temp = mean(weighted_temp),
        mean_precip = mean(weighted_precip)
    )

cross_county_seasonal <-
    final %>% 
    filter(
        sex == "all",
        age_group == "all_ages",
        race_eth == "all",
        marital == "all",
    ) %>% 
    mutate(
        season =
            case_when(
                month == 1 | month == 2 | month == 3 ~ "Winter",
                month == 4 | month == 5 | month == 6 ~ "Spring",
                month == 7 | month == 8 | month == 9 ~ "Summer",
                month == 10 | month == 11 | month == 12 ~ "Fall"
            )
    ) %>% 
    group_by(fipsihme, season) %>% 
    summarise(
        mean_pop = mean(pop),
        mean_monthly_deaths = mean(n_deaths),
        mean_deathrate = mean(rate),
        mean_pm2.5 = mean(mean_pm2.5),
        mean_temp = mean(weighted_temp),
        mean_precip = mean(weighted_precip)
    )
 
table2_data <-
    rbind(
        #population
        c(
            "Population",
            cross_county_annual %>% 
                pull(mean_pop) %>% 
                mean() %>% 
                round() %>% 
                format(big.mark = ","),
            cross_county_annual %>% 
                pull(mean_pop) %>% 
                sd() %>% 
                round() %>% 
                format(big.mark = ","),
            cross_county_annual %>% 
                pull(mean_pop) %>% 
                quantile(c(.05, .25, .5, .75, .95, .99)) %>% 
                as.numeric() %>% 
                round() %>% 
                format(big.mark = ",")
        ),
        #all-cause mortality rate, age-adjusted
        c(
            "Age-adjusted mortality rate (per 100,000)",
            cross_county_annual %>% 
                pull(mean_deathrate) %>% 
                mean() %>% 
                round() %>% 
                format(big.mark = ","),
            cross_county_annual %>% 
                pull(mean_deathrate) %>% 
                sd() %>% 
                round() %>% 
                format(big.mark = ","),
            cross_county_annual %>% 
                pull(mean_deathrate) %>% 
                quantile(c(.05, .25, .5, .75, .95, .99)) %>% 
                as.numeric() %>% 
                round() %>% 
                format(big.mark = ",")
        ),
        #death counts per year
        c(
            "Monthly deaths",
            cross_county_annual %>% 
                pull(mean_monthly_deaths) %>% 
                mean() %>% 
                round() %>% 
                format(big.mark = ","),
            cross_county_annual %>% 
                pull(mean_monthly_deaths) %>% 
                sd() %>% 
                round() %>% 
                format(big.mark = ","),
            cross_county_annual %>% 
                pull(mean_monthly_deaths) %>% 
                quantile(c(.05, .25, .5, .75, .95, .99)) %>% 
                as.numeric() %>% 
                round() %>% 
                format(big.mark = ",")
        ),
        #mean wfpm2.5 exposure
        c(
            "Mean wildfire smoke PM2.5 (µg/m3)",
            cross_county_annual %>% 
                pull(mean_pm2.5) %>% 
                mean() %>% 
                round(digits = 2) %>% 
                format(big.mark = ","),
            cross_county_annual %>% 
                pull(mean_pm2.5) %>% 
                sd() %>% 
                round(digits = 2) %>% 
                format(big.mark = ","),
            cross_county_annual %>% 
                pull(mean_pm2.5) %>% 
                quantile(c(.05, .25, .5, .75, .95, .99)) %>% 
                as.numeric() %>% 
                round(digits = 2) %>% 
                format(big.mark = ",")
        ),
        #mean temp
        c(
            "Mean population-weighted temp (C)",
            cross_county_annual %>% 
                pull(mean_temp) %>% 
                mean(na.rm=T) %>% 
                round(digits = 2) %>% 
                format(big.mark = ","),
            cross_county_annual %>% 
                pull(mean_temp) %>% 
                sd(na.rm=T) %>% 
                round(digits = 2) %>% 
                format(big.mark = ","),
            cross_county_annual %>% 
                pull(mean_temp) %>% 
                quantile(c(.05, .25, .5, .75, .95, .99), na.rm=T) %>% 
                as.numeric() %>% 
                round(digits = 2) %>% 
                format(big.mark = ",")
        ),
        #mean precip
        c(
            "Mean population-weighted precip (mm)",
            cross_county_annual %>% 
                pull(mean_precip) %>% 
                mean(na.rm=T) %>% 
                round(digits = 2) %>% 
                format(big.mark = ","),
            cross_county_annual %>% 
                pull(mean_precip) %>% 
                sd(na.rm=T) %>% 
                round(digits = 2) %>% 
                format(big.mark = ","),
            cross_county_annual %>% 
                pull(mean_precip) %>% 
                quantile(c(.05, .25, .5, .75, .95, .99), na.rm=T) %>% 
                as.numeric() %>% 
                round(digits = 2) %>% 
                format(big.mark = ",")
        )
    ) %>% 
    data.frame() %>% 
    rename(measure = X1, 
           Mean = X2,
           `Std. Dev.` = X3,
           `5th percentile` = X4, 
           `25th percentile` = X5, 
           `50th percentile` = X6, 
           `75th percentile` = X7, 
           `95th percentile` = X8,
           `99th percentile` = X9)
    
table2 <-
    table2_data %>%
    gt(rowname_col = "measure") %>% 
    tab_header(
        title = "Table 2: Distribution of county-specific means, Jan. 2006 - Feb. 2020"
    ) %>% 
    tab_stubhead(label = "Variable")

#May get a bug on this line--need to restart R in order for it to work, see
#this page: https://stackoverflow.com/questions/73964325/r-gt-gtsave-error-in-sclose-attempt-to-apply-non-function
gtsave(table2, "plots/table2.png")

# Table 3: across-time distribution of smoke PM2.5 and mortality
# all commented out, time series is better representation
# cross_time <-
#     final %>% 
#     filter(
#         sex == "all",
#         age_group == "15_and_up",
#         race_eth == "all",
#         marital == "all",
#         
#         year < 2020 #only two months so not reasonable to include it
#     ) %>% 
#     group_by(year) %>% 
#     summarise(
#         mean_pop = mean(pop),
#         deaths_per_year = mean(n_deaths),
#         mean_deathrate = mean(rate),
#         mean_pm2.5 = mean(mean_pm2.5)
#     ) %>% 
#     rename(
#         Population = mean_pop,
#         `Death counts per year` = deaths_per_year,
#         `Age-adjusted all-cause mortality rate`= mean_deathrate,
#         `Mean wildfire smoke PM2.5 (µg/m3)` = mean_pm2.5
#     ) %>% 
#     pivot_longer(cols= -1) %>% 
#     pivot_wider(names_from = "year", values_from = "value") %>% 
#     rename(measure = name)
# 
# table3_data <-
#     rbind(
#         cross_time %>% 
#             slice(1:3) %>% 
#             mutate(across(where(is.numeric), round)),
#         cross_time %>% 
#             slice(4) %>% 
#             mutate(across(where(is.numeric), round, 2))
#     )
# 
# table3 <-
#     table3_data %>%
#     gt(rowname_col = "measure") %>% 
#     tab_header(
#         title = "Table 3: Annual mean values, 2006-2019"
#     ) %>% 
#     tab_stubhead(label = "Variable") %>% 
#     fmt_number(
#         columns = 2:12,
#         rows = 1:3,
#         decimals = 0,
#         use_seps = TRUE
#     )
# 
# gtsave(table3, "plots/table3.png")



