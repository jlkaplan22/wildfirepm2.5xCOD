#### Fig 1 ####

maplegend_titletextsize <- 16
maplegend_textsize <- 14

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
        age_group == "all_ages",
        race_eth == "all",
        marital == "all",
        
        year < 2020 #only two months so not reasonable to include it
    ) %>% 
    group_by(fipsihme) %>% 
    summarise(
        mean_pop = mean(pop),
        deaths_per_year = mean(n_deaths),
        mean_deathrate = mean(rate),
        months_over_x = sum(mean_pm2.5 > .5),
        mean_pm2.5 = mean(mean_pm2.5)
    )

#deathrate_breaks <- quantile(map_data$mean_deathrate, seq(0, 1, .2))
deathrate_breaks <- c(0, 50, 60, 70, 80, 90, Inf)
#smokepm2.5_breaks <- quantile(map_data$mean_pm2.5, seq(0, 1, .2))
smokepm2.5_breaks <- c(0, 20, 30, 40, 50, Inf)

map_data <-
    map_data %>% 
    mutate(
        deathrate_bin = cut(mean_deathrate, breaks = deathrate_breaks),
        smokepm2.5_bin = cut(months_over_x, breaks = smokepm2.5_breaks)
    ) %>% 
    left_join(
        counties_sf
    ) %>% 
    st_as_sf() #key step to recover sf geometry for some reason

#deathrate_quintiles <- map_data$deathrate_bin %>% levels()
deathrate_buckets <- map_data$deathrate_bin %>% levels()
deathrate_labels <- deathrate_buckets %>% str_sub(2, -2) %>% str_replace(",", "-")

#smokepm2.5_quintiles <- map_data$smokepm2.5_bin %>% levels()
smokepm2.5_buckets <- map_data$smokepm2.5_bin %>% levels()
smokepm2.5_labels <- smokepm2.5_buckets %>% str_sub(2, -2) %>% str_replace(",", "-")

# Smoke PM2.5 map
smokepm2.5_map <-
    map_data %>% 
    mutate(smokepm2.5_bin = factor(smokepm2.5_bin, labels = smokepm2.5_labels)) %>% 
    #filter(fipsihme == "01001" | fipsihme == "01003") %>% #line for testing
    ggplot(aes(fill = smokepm2.5_bin)) +
    geom_sf(color = NA) + #color=NA to remove county borders
    scale_fill_brewer(palette = "OrRd") +
    theme_void() +
    guides(fill=guide_legend(title="Months over 0.5 µg/m3 smoke PM2.5")) +
    theme(
        legend.key.height = unit(.1, 'cm'), #change legend key height
        legend.key.width = unit(.25, 'cm'), #change legend key width
        legend.title = element_text(size=maplegend_titletextsize), #change legend title font size
        legend.text = element_text(size=maplegend_textsize), #change legend text font size
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
    scale_fill_brewer(palette = "Purples") +
    theme_void() +
    guides(fill=guide_legend(title="Age-adjusted mortality rate (deaths per 100,000)")) +
    theme(
            legend.key.height = unit(.1, 'cm'), #change legend key height
            legend.key.width = unit(.25, 'cm'), #change legend key width
            legend.title = element_text(size=maplegend_titletextsize), #change legend title font size
            legend.text = element_text(size=maplegend_textsize), #change legend text font size
            legend.position = "bottom"
        )

ggsave(filename = "plots/fig1_deathrates.png", plot = mortalityrate_map, device = "png", dpi = 200)

# Shape data for time series use
time_series_data <-
    final %>% 
    filter(
        sex == "all",
        age_group == "all_ages",
        race_eth == "all",
        marital == "all",
        
        year < 2020 #only two months so not reasonable to include it
    ) %>% 
    mutate(
        yearmonth = paste(year, month, sep="-") %>% ym(),
        
        # season = 
        #     case_when(
        #         month == 1 | month == 2 | month == 3 ~ "Winter",
        #         month == 4 | month == 5 | month == 6 ~ "Spring",
        #         month == 7 | month == 8 | month == 9 ~ "Summer",
        #         month == 10 | month == 11 | month == 12 ~ "Fall"
        #     ) %>% as.factor()
    ) %>% 
    group_by(yearmonth) %>% 
    summarise(
        mean_deathrate = mean(rate),
        mean_pm2.5 = mean(mean_pm2.5)
    )

# Smoke PM2.5 time series
pm2.5_timeseries <-
    time_series_data %>% 
    ggplot(aes(x = yearmonth, y = mean_pm2.5)) +
    geom_line(linewidth = .5) +
    #scale_x_continuous(breaks = c(seq(2006, 2019, 2)), labels=c(seq(2006, 2019, 2))) +
    theme_minimal() +
    #scale_color_brewer(palette = "Set1") + 
    labs(
        #color = "Season",
        x = "",
        y = "Wildfire smoke PM2.5 (µg/m3)"
    ) +
    theme(
        #panel.grid.minor = element_blank(),
        axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20)
    )

ggsave(filename = "plots/fig1_pm2.5_timeseries.png", plot = pm2.5_timeseries, device = "png", dpi = 200, height = 8.47, width = 12)


# Mortality rate time series
deathrate_timeseries <-
    time_series_data %>% 
    ggplot(aes(x = yearmonth, y = mean_deathrate)) +
    geom_line(linewidth = .5) +
    #scale_x_continuous(breaks = c(seq(2006, 2019, 1))) +
    theme_minimal() +
    #scale_color_brewer(palette = "Set1") + 
    labs(
        #color = "Season",
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

#Histogram to show distribution of wfpm2.5
options(scipen=999) #removes scientific notation for following plot

county_smokePM_features$breaks <- cut(county_smokePM_features$mean_pm2.5, breaks=c(0,5,10,25,50,Inf), include.lowest=TRUE)


hist_data <-
    county_smokePM_features %>% 
    mutate(
        bars = cut(mean_pm2.5, breaks=c(seq(0, 80, 5), Inf), include.lowest=TRUE),
        groupcolors = cut(mean_pm2.5, breaks=c(0,5,10,25,50,Inf), include.lowest=TRUE)
    )

legend_labels <-
    hist_data %>% 
    group_by(groupcolors) %>% 
    summarise(n = n(), percent = n / nrow(hist_data) * 100 %>% as.numeric()) %>% 
    pull(percent) %>% 
    formatC(digits = 4) %>% 
    cbind(
        c("0-5 µg/m3", "5-10 µg/m3", "10-25 µg/m3", "25-50 µg/m3", "50+ µg/m3"),
        .
    ) %>% 
    data.frame() %>% 
    mutate(
        percent = paste0("(", `.`, "%", ")"),
        label = paste(`V1`, percent, sep = " ")
    ) %>% 
    pull(label)
    
    
    unite(`V1`:`.`, sep = " ")

pm2.5_histogram <-
    hist_data %>% 
    ggplot(aes(bars, fill=groupcolors)) +
    #geom_histogram(color="black", binwidth=5, closed="left") +
    geom_bar(color="black") +
    scale_x_discrete(labels = c(seq(0, 75, 5), "80+")) +
    scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), 
                       labels=scales::comma,
                       breaks=c(0, 1, 10, 100, 1000, 10000, 100000, 500000)) +
    theme_minimal() +
    theme(
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=20)
    ) +
    scale_fill_brewer(palette = "YlOrRd", labels=legend_labels) +
    xlab("Monthly mean smoke PM2.5 (µg/m3)") +
    ylab("County-month observations")

ggsave(filename = "plots/fig1_pm2.5_histogram.png", plot = pm2.5_histogram, device = "png", dpi = 200, height = 6, width = 7)

