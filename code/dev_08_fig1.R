#### Fig 1 ####

#Load data
source("code/quickprepfinaldata_MS.R")

# Shape data for map use
counties_sf <- 
    counties(year = 2015) %>% 
    filter(STATEFP %in% non_CONUS_FIPS == F) %>% 
    left_join(
        ihme_fips %>% dplyr::select(orig_fips, ihme_fips),
        by=c("COUNTYFP" = "orig_fips")
    ) %>% 
    dplyr::select(fipsihme = GEOID)

map_data <-
    final %>% 
    filter(
        sex == "all",
        age_group == "15_and_up",
        race_eth == "all",
        marital == "all",
        
        year < 2020 #only two months so not reasonable to include it
    ) %>% 
    group_by(fipsihme) %>% 
    summarise(
        mean_pop = mean(pop),
        deaths_per_year = mean(n_deaths),
        mean_deathrate = mean(rate),
        mean_pm2.5 = mean(mean_pm2.5)
    )

deathrate_breaks <- quantile(map_data$mean_deathrate, seq(0, 1, .2))
smokepm2.5_breaks <- quantile(map_data$mean_pm2.5, seq(0, 1, .2))


map_data <-
    map_data %>% 
    mutate(
        deathrate_bin = cut(mean_deathrate, breaks = deathrate_breaks),
        smokepm2.5_bin = cut(mean_pm2.5, breaks = smokepm2.5_breaks)
    ) %>% 
    left_join(
        counties_sf
    ) %>% 
    st_as_sf() #key step to recover sf geometry for some reason

deathrate_quintiles <- map_data$deathrate_bin %>% levels()
deathrate_labels <- deathrate_quintiles %>% str_sub(2, -2) %>% str_replace(",", "-")

smokepm2.5_quintiles <- map_data$smokepm2.5_bin %>% levels()
smokepm2.5_labels <- smokepm2.5_quintiles %>% str_sub(2, -2) %>% str_replace(",", "-")

# Smoke PM2.5 map
smokepm2.5_map <-
    map_data %>% 
    mutate(smokepm2.5_bin = factor(smokepm2.5_bin, labels = smokepm2.5_labels)) %>% 
    #filter(fipsihme == "01001" | fipsihme == "01003") %>% #line for testing
    ggplot(aes(fill = smokepm2.5_bin)) +
    geom_sf(color = NA) + #color=NA to remove county borders
    scale_fill_brewer(palette = "Purples") +
    theme_void() +
    guides(fill=guide_legend(title="Mean wildfire smoke PM2.5 (µg/m3)")) +
    theme(
        legend.key.height = unit(.1, 'cm'), #change legend key height
        legend.key.width = unit(.25, 'cm'), #change legend key width
        legend.title = element_text(size=12), #change legend title font size
        legend.text = element_text(size=11), #change legend text font size
        legend.position = "bottom"
    )

ggsave(filename = "plots/fig1_smokepm2.5.png", plot = smokepm2.5_map, device = "png", dpi = 200)

# Mortality rate map
mortalityrate_map <-
    map_data %>% 
    mutate(deathrate_bin = factor(deathrate_bin, labels = deathrate_labels)) %>% 
    #filter(fipsihme == "01001" | fipsihme == "01003") %>% #line for testing
    ggplot(aes(fill = deathrate_bin)) +
    geom_sf(color = NA) + #color=NA to remove county borders
    scale_fill_brewer(palette = "OrRd") +
    theme_void() +
    guides(fill=guide_legend(title="Age-adjusted mortality rate (deaths per 10,000)")) +
    theme(
            legend.key.height = unit(.1, 'cm'), #change legend key height
            legend.key.width = unit(.25, 'cm'), #change legend key width
            legend.title = element_text(size=12), #change legend title font size
            legend.text = element_text(size=11), #change legend text font size
            legend.position = "bottom"
        )

ggsave(filename = "plots/fig1_deathrates.png", plot = mortalityrate_map, device = "png", dpi = 200)

# Shape data for time series use
time_series_data <-
    final %>% 
    filter(
        sex == "all",
        age_group == "15_and_up",
        race_eth == "all",
        marital == "all",
        
        year < 2020 #only two months so not reasonable to include it
    ) %>% 
    mutate(
        season = 
            case_when(
                month == 1 | month == 2 | month == 3 ~ "Winter",
                month == 4 | month == 5 | month == 6 ~ "Spring",
                month == 7 | month == 8 | month == 9 ~ "Summer",
                month == 10 | month == 11 | month == 12 ~ "Fall"
            ) %>% as.factor()
    ) %>% 
    group_by(year, season) %>% 
    summarise(
        mean_deathrate = mean(rate),
        mean_pm2.5 = mean(mean_pm2.5)
    )

# Smoke PM2.5 time series
pm2.5_timeseries <-
    time_series_data %>% 
    ggplot(aes(x = year, y = mean_pm2.5, color = season)) +
    geom_line(linewidth = 2) +
    scale_x_continuous(breaks = c(seq(2009, 2019, 1))) +
    theme_minimal() +
    scale_color_brewer(palette = "Set1") + 
    labs(
        color = "Season",
        x = "",
        y = "Wildfire smoke PM2.5 (µg/m3)"
    ) +
    theme(
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20)
    )

ggsave(filename = "plots/fig1_pm2.5_timeseries.png", plot = pm2.5_timeseries, device = "png", dpi = 200, height = 8.47, width = 12)


# Mortality rate time series
deathrate_timeseries <-
    time_series_data %>% 
    ggplot(aes(x = year, y = mean_deathrate, color = season)) +
    geom_line(linewidth = 2) +
    scale_x_continuous(breaks = c(seq(2009, 2019, 1))) +
    theme_minimal() +
    scale_color_brewer(palette = "Set1") + 
    labs(
        color = "Season",
        x = "",
        y = "Age-adjusted mortality per 10,000"
    ) +
    theme(
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20)
    )

ggsave(filename = "plots/fig1_deathrate_timeseries.png", plot = deathrate_timeseries, device = "png", dpi = 200, height = 8.47, width = 12)






