#### Fig 3--effect of study window on model output ####

#Fig 3a: 5-year window with iteratively added years
output_a <- as.data.frame(matrix(nrow=10, ncol=6))
colnames(output_a) <- c("startyear", "endyear", "mean_pm2.5", "se_county_cluster", "se_iid", "se_hetero")

for (i in 1:11) {
    indexyear <- 2008 + i
    
    year_restricted_df <-
        final %>% 
        filter(year <= indexyear)
    
    controls <- "ns(weighted_temp, df=3) + weighted_precip"
    fes <- "fipsihme^month + year"
    
    output_a[i,] <- 
        c(
            "2006", indexyear,
            modeler(year_restricted_df, "mean_pm2.5", controls, fes, year_restricted_df$fipsihme)
        )
}

fig3a_mod_outputs <-
    output_a %>% 
    mutate(
        year_range = paste(startyear, endyear, sep="-"),
        point_est = exp(mean_pm2.5),
        CI_lower_clustered = exp(mean_pm2.5 - qnorm(0.975) * se_county_cluster),
        CI_upper_clustered = exp(mean_pm2.5 + qnorm(0.975) * se_county_cluster),
        CI_lower_iid = exp(mean_pm2.5 - qnorm(0.975) * se_iid),
        CI_upper_iid = exp(mean_pm2.5 + qnorm(0.975) * se_iid),
        .keep=c("all")
    ) %>% 
    cbind(seq(1:nrow(.))) %>% 
    rename(mod_num = `seq(1:nrow(.))`)

coefs_a <-
    fig3a_mod_outputs %>% 
    ggplot(aes(x = mod_num, y = point_est - 1)) + 
    geom_linerange(aes(ymin = CI_lower_clustered - 1, ymax = CI_upper_clustered - 1), lwd=.2) +
    geom_linerange(aes(ymin = CI_lower_iid - 1, ymax = CI_upper_iid - 1), lwd=.6, color = "cornflowerblue") +
    geom_point(size=.5) +
    theme_minimal() +
    geom_hline(yintercept = 0, colour = DEFAULT_COLOR, lty = 2, size=0.25) + 
    theme(
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0,1,0,1), "cm")
    )

years_a <-
    fig3a_mod_outputs %>% 
    mutate(
        mod_num = mod_num,
        `2006` = ifelse(startyear <= 2006 & endyear >= 2006, 1, 0),
        `2007` = ifelse(startyear <= 2007 & endyear >= 2007, 1, 0),
        `2008` = ifelse(startyear <= 2008 & endyear >= 2008, 1, 0),
        `2009` = ifelse(startyear <= 2009 & endyear >= 2009, 1, 0),
        `2010` = ifelse(startyear <= 2010 & endyear >= 2010, 1, 0),
        `2011` = ifelse(startyear <= 2011 & endyear >= 2011, 1, 0),
        `2012` = ifelse(startyear <= 2012 & endyear >= 2012, 1, 0),
        `2013` = ifelse(startyear <= 2013 & endyear >= 2013, 1, 0),
        `2014` = ifelse(startyear <= 2014 & endyear >= 2014, 1, 0),
        `2015` = ifelse(startyear <= 2015 & endyear >= 2015, 1, 0),
        `2016` = ifelse(startyear <= 2016 & endyear >= 2016, 1, 0),
        `2017` = ifelse(startyear <= 2017 & endyear >= 2017, 1, 0),
        `2018` = ifelse(startyear <= 2018 & endyear >= 2018, 1, 0),
        `2019` = ifelse(startyear <= 2019 & endyear >= 2019, 1, 0),
        .keep = c("none")
    ) %>% 
    pivot_longer(`2006`:`2019`, names_to="year", values_to="value")

height <- .4
width <- .5
years_a_tiles <-
    years_a %>% 
    ggplot(aes(x = mod_num, y = as.factor(year) %>% fct_rev(), fill = value)) + 
    geom_tile(color = "gray", height = height, width = width) +
    scale_alpha_manual(NULL, values = c(0, 0, 1, .4)) +
    theme_minimal() + 
    scale_fill_gradient(low = "white", high = "dodgerblue3") +
    theme(
        legend.position = "none",
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,1,0,1), "cm")
    )

fig3a <- coefs_a + years_a_tiles + plot_layout(ncol = 1, heights = c(2,1))
ggsave(filename = "plots/fig3a.png", plot = fig3a, device = "png", dpi = 200, height = 5, width = 5)


#Fig 3b: moving 5-year window
output_b <- as.data.frame(matrix(nrow=9, ncol=6))
colnames(output_b) <- c("startyear", "endyear", "mean_pm2.5", "se_county_cluster", "se_iid", "se_hetero")

range <- 3

for (i in 1:(2020-2006-range)) {
    startyear <- 2005 + i
    endyear <- startyear + range

    year_restricted_df <-
        final %>% 
        filter(year >= startyear, year <= endyear)
    
    controls <- "ns(weighted_temp, df=3) + weighted_precip"
    fes <- "fipsihme^month + year"
    
    output_b[i,] <- 
        c(
            startyear, endyear,
            modeler(year_restricted_df, "mean_pm2.5", controls, fes, year_restricted_df$fipsihme)
        )
}

fig3b_mod_outputs <-
    output_b %>% 
    mutate(
        year_range = paste(startyear, endyear, sep="-"),
        point_est = exp(mean_pm2.5),
        CI_lower_clustered = exp(mean_pm2.5 - qnorm(0.975) * se_county_cluster),
        CI_upper_clustered = exp(mean_pm2.5 + qnorm(0.975) * se_county_cluster),
        CI_lower_iid = exp(mean_pm2.5 - qnorm(0.975) * se_iid),
        CI_upper_iid = exp(mean_pm2.5 + qnorm(0.975) * se_iid),
        .keep=("all")
    ) %>% 
    cbind(seq(1:nrow(.))) %>% 
    rename(mod_num = `seq(1:nrow(.))`)

coefs_b <-
    fig3b_mod_outputs %>% 
    ggplot(aes(x = year_range, y = point_est - 1)) + 
    geom_linerange(aes(ymin = CI_lower_clustered - 1, ymax = CI_upper_clustered - 1), lwd=.2) +
    geom_linerange(aes(ymin = CI_lower_iid - 1, ymax = CI_upper_iid - 1), lwd=.6, color = "cornflowerblue") +
    geom_point(size=.5) +
    theme_minimal() +
    geom_hline(yintercept = 0, colour = DEFAULT_COLOR, lty = 2, size=0.25) + 
    theme(
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0,1,0,1), "cm")
    )

years_b <-
    fig3b_mod_outputs %>% 
    mutate(
        mod_num = mod_num,
        `2006` = ifelse(startyear <= 2006 & endyear >= 2006, 1, 0),
        `2007` = ifelse(startyear <= 2007 & endyear >= 2007, 1, 0),
        `2008` = ifelse(startyear <= 2008 & endyear >= 2008, 1, 0),
        `2009` = ifelse(startyear <= 2009 & endyear >= 2009, 1, 0),
        `2010` = ifelse(startyear <= 2010 & endyear >= 2010, 1, 0),
        `2011` = ifelse(startyear <= 2011 & endyear >= 2011, 1, 0),
        `2012` = ifelse(startyear <= 2012 & endyear >= 2012, 1, 0),
        `2013` = ifelse(startyear <= 2013 & endyear >= 2013, 1, 0),
        `2014` = ifelse(startyear <= 2014 & endyear >= 2014, 1, 0),
        `2015` = ifelse(startyear <= 2015 & endyear >= 2015, 1, 0),
        `2016` = ifelse(startyear <= 2016 & endyear >= 2016, 1, 0),
        `2017` = ifelse(startyear <= 2017 & endyear >= 2017, 1, 0),
        `2018` = ifelse(startyear <= 2018 & endyear >= 2018, 1, 0),
        `2019` = ifelse(startyear <= 2019 & endyear >= 2019, 1, 0),
        .keep = c("none")
    ) %>% 
    pivot_longer(`2006`:`2019`, names_to="year", values_to="value")

years_b_tiles <-
    years_b %>% 
    ggplot(aes(x = mod_num, y = as.factor(year) %>% fct_rev(), fill = value)) + 
    geom_tile(color = "gray", height = height, width = width) +
    scale_alpha_manual(NULL, values = c(0, 0, 1, .4)) +
    theme_minimal() + 
    scale_fill_gradient(low = "white", high = "dodgerblue3") +
    theme(
        legend.position = "none",
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,1,0,1), "cm")
    )

fig3b <- coefs_b + years_b_tiles + plot_layout(ncol = 1, heights = c(2,1))
ggsave(filename = "plots/fig3b.png", plot = fig3b, device = "png", dpi = 200, height = 5, width = 5)
