#### Generate descriptive tables 1 and 2 ####

#Load data
source("code/quickprepfinaldata_MS.R")

# Table 1: across-county distribution of smoke PM2.5 and mortality
cross_county <-
    final %>% 
    filter(
        sex == "all",
        age_group == "15_and_up",
        race_eth == "all",
        marital == "all",
    ) %>% 
    group_by(fipsihme) %>% 
    summarise(
        mean_pop = mean(pop),
        deaths_per_year = mean(n_deaths),
        mean_deathrate = mean(rate),
        mean_pm2.5 = mean(mean_pm2.5)
    )
 
table1_data <-
    rbind(
        #population
        c(
            "Population",
            cross_county %>% 
                pull(mean_pop) %>% 
                mean() %>% 
                round() %>% 
                format(big.mark = ","),
            cross_county %>% 
                pull(mean_pop) %>% 
                quantile(c(.05, .25, .5, .75, .95)) %>% 
                as.numeric() %>% 
                round() %>% 
                format(big.mark = ",")
        ),
        #all-cause mortality rate, age-adjusted
        c(
            "Age-adjusted all-cause mortality rate",
            cross_county %>% 
                pull(mean_deathrate) %>% 
                mean() %>% 
                round() %>% 
                format(big.mark = ","),
            cross_county %>% 
                pull(mean_deathrate) %>% 
                quantile(c(.05, .25, .5, .75, .95)) %>% 
                as.numeric() %>% 
                round() %>% 
                format(big.mark = ",")
        ),
        #death counts per year
        c(
            "Death counts per year",
            cross_county %>% 
                pull(deaths_per_year) %>% 
                mean() %>% 
                round() %>% 
                format(big.mark = ","),
            cross_county %>% 
                pull(deaths_per_year) %>% 
                quantile(c(.05, .25, .5, .75, .95)) %>% 
                as.numeric() %>% 
                round() %>% 
                format(big.mark = ",")
        ),
        #mean wfpm2.5 exposure
        c(
            "Mean wildfire smoke PM2.5 (µg/m3)",
            cross_county %>% 
                pull(mean_pm2.5) %>% 
                mean() %>% 
                round(digits = 2) %>% 
                format(big.mark = ","),
            cross_county %>% 
                pull(mean_pm2.5) %>% 
                quantile(c(.05, .25, .5, .75, .95)) %>% 
                as.numeric() %>% 
                round(digits = 2) %>% 
                format(big.mark = ",")
        )
    ) %>% 
    data.frame() %>% 
    rename(measure = X1, 
           Mean = X2, 
           `5th percentile` = X3, 
           `25th percentile` = X4, 
           `50th percentile` = X5, 
           `75th percentile` = X6, 
           `95th percentile` = X7)
    
table1 <-
    table1_data %>%
    gt(rowname_col = "measure") %>% 
    tab_header(
        title = "Table 1: Distribution of county-specific means, Jan. 2006 - Feb. 2020"
    ) %>% 
    tab_stubhead(label = "Variable")

#May get a bug on this line--need to restart R in order for it to work, see
#this page: https://stackoverflow.com/questions/73964325/r-gt-gtsave-error-in-sclose-attempt-to-apply-non-function
gtsave(table1, "plots/table1.png")

# Table 2: across-time distribution of smoke PM2.5 and mortality
cross_time <-
    final %>% 
    filter(
        sex == "all",
        age_group == "15_and_up",
        race_eth == "all",
        marital == "all",
        
        year < 2020 #only two months so not reasonable to include it
    ) %>% 
    group_by(year) %>% 
    summarise(
        mean_pop = mean(pop),
        deaths_per_year = mean(n_deaths),
        mean_deathrate = mean(rate),
        mean_pm2.5 = mean(mean_pm2.5)
    ) %>% 
    rename(
        Population = mean_pop,
        `Death counts per year` = deaths_per_year,
        `Age-adjusted all-cause mortality rate`= mean_deathrate,
        `Mean wildfire smoke PM2.5 (µg/m3)` = mean_pm2.5
    ) %>% 
    pivot_longer(cols= -1) %>% 
    pivot_wider(names_from = "year", values_from = "value") %>% 
    rename(measure = name)

table2_data <-
    rbind(
        cross_time %>% 
            slice(1:3) %>% 
            mutate(across(where(is.numeric), round)),
        cross_time %>% 
            slice(4) %>% 
            mutate(across(where(is.numeric), round, 2))
    )

table2 <-
    table2_data %>%
    gt(rowname_col = "measure") %>% 
    tab_header(
        title = "Table 2: Annual mean values, 2006-2019"
    ) %>% 
    tab_stubhead(label = "Variable") %>% 
    fmt_number(
        columns = 2:12,
        rows = 1:3,
        decimals = 0,
        use_seps = TRUE
    )

gtsave(table2, "plots/table2.png")



