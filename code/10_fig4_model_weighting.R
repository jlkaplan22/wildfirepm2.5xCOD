#### TWFEs with varying model specifications ####
#Comparing weighting model by county population vs not

x <-
    c(
        "popw_mean_pm2.5",
        "popw_daysover0",
        "popw_daysover5"
    )

fes <- 
    c(
        "fipsihme + yearmonth", 
        "fipsihme^month + year", 
        "fipsihme + year",
        "fipsihme + yearmonth + statefips^month",
        "fipsihme + yearmonth + statefips^year"
    )

temp <- 
    c("",
      # "areaw_monthly_mean_tempF", 
      "popw_monthly_mean_tempF", 
      "ns(popw_monthly_mean_tempF, df=3)", 
      "ns(popw_monthly_mean_tempF, df=5)",
      "popw_days_u30 + popw_days_30to40 + popw_days_50to60 + popw_days_60to70 + popw_days_70to80 + popw_days_80to90 + popw_days_90to100 + popw_days_o100"
    )

precip <- 
    c("", 
      "popw_monthly_mean_precip"
      # "areaw_monthly_mean_precip", 
      # "popw_days_w_precip",
      # "popw_days_above_30mm",
      # "ns(popw_monthly_mean_precip, df=3)", 
      # "ns(popw_monthly_mean_precip, df=5)"
    )

grid <- 
    expand.grid(x = x, fes = fes, temp = temp, precip = precip) %>% 
    tibble() %>% 
    mutate(
        controls = 
            dplyr::case_when(
                temp == "" & precip == "" ~ "",
                temp == "" & precip != "" ~ as.character(precip),
                precip == "" & temp != "" ~ as.character(temp),
                TRUE ~ paste0(as.character(precip), "+", as.character(temp))
            ),
        .keep = c("unused")
    ) %>% 
    cbind(tibble(model_number = seq(1:nrow(.)))) %>% 
    dplyr::select(model_number, x, fes, controls)

output <- as.data.frame(matrix(nrow=nrow(grid)*2, ncol=9))
colnames(output) <- c("model_number", "weight", "mean_pm2.5", 
                      "se_county_cluster", "pval_county_cluster",
                      "se_iid", "pval_iid",
                      "se_hetero", "pval_hetero")

for (i in 1:nrow(grid)) {
    fes <- grid$fes[i]
    controls <- grid$controls[i]
    
    model_output_unweighted <- modeler_weighted(final, x, controls, fes, final$fipsihme, weights=FALSE)
    model_output_weighted <- modeler_weighted(final, x, controls, fes, final$fipsihme, weights=TRUE)
    
    
    output[i*2 - 1,] <- 
        c(
            grid$model_number[i],
            "Unweighted",
            model_output_unweighted
        )
    output[i*2,] <- 
        c(
            grid$model_number[i],
            "Weighted",
            model_output_weighted
        )
}

fig2d_mod_outputs <-
    output %>% 
    mutate(
        model_number = model_number,
        #exponentiate
        point_est = exp(mean_pm2.5),
        CI_lower_clustered = exp(mean_pm2.5 - qnorm(0.975) * se_county_cluster),
        CI_upper_clustered = exp(mean_pm2.5 + qnorm(0.975) * se_county_cluster),
        CI_lower_iid = exp(mean_pm2.5 - qnorm(0.975) * se_iid),
        CI_upper_iid = exp(mean_pm2.5 + qnorm(0.975) * se_iid),
        
        #convert to interpretable percentages
        point_est_percent = (point_est - 1) * 100,
        CI_lower_clustered_percent = (CI_lower_clustered - 1) * 100,
        CI_upper_clustered_percent = (CI_upper_clustered - 1) * 100,
        CI_lower_iid_percent = (CI_lower_iid - 1) * 100,
        CI_upper_iid_percent = (CI_upper_iid - 1) * 100,
        
        preferred_mod = ifelse(model_number==26 & weight=="Weighted", 1, 0)
    ) %>% 
    arrange(point_est) %>% 
    cbind(
        seq(1:nrow(.))
    ) %>% 
    rename(model_rank = `seq(1:nrow(.))`)

# Trying to create dumbbell plot:
weighted_mods <-
    fig2d_mod_outputs %>% 
    filter(weight == "Weighted") %>% 
    dplyr::select(-model_rank) %>% 
    arrange(point_est) %>% 
    cbind(
        seq(1:nrow(.))
    ) %>% 
    rename(model_rank = `seq(1:nrow(.))`)

unweighted_mods <-
    fig2d_mod_outputs %>% 
    filter(weight == "Unweighted") %>% 
    dplyr::select(-model_rank) %>% 
    left_join(
        weighted_mods %>% dplyr::select(model_number, model_rank)
    ) %>% 
    arrange(model_rank)

dumbbell_data <-
    rbind(weighted_mods, unweighted_mods)


fig4c <-
    ggplot(dumbbell_data) +
    geom_segment(data = unweighted_mods,
                 aes(x = point_est_percent, y = model_rank,
                     xend = weighted_mods$point_est_percent, yend = weighted_mods$model_rank),
                 color = "#aeb6bf", alpha = .7, size = 1) +
    scale_x_break(c(-1.5, -.15)) +
    geom_point(aes(x = point_est_percent, y = model_rank, color = weight), size = 1, show.legend = TRUE) +
    geom_vline(xintercept = 0, colour = DEFAULT_COLOR, lty = 2, size=0.25) + 
    theme_minimal() +
    scale_color_brewer(palette = "Set2") +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        #legend.position = c(0.78, 0.75),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()
    ) +
    xlab("% Change in Mortality Rate")
    #Spotlight the prefered model (coding manually, will need to adjust if models change)
    # annotate("rect",
    #          ymin = dumbbell_data %>% filter(preferred_mod==1) %>% pull(model_rank) - .5,
    #          ymax = dumbbell_data %>% filter(preferred_mod==1) %>% pull(model_rank) + .5,
    #          xmin = dumbbell_data %>% filter(preferred_mod==1) %>% pull(CI_lower_clustered_percent),
    #          xmax = dumbbell_data %>% filter(preferred_mod==1) %>% pull(CI_upper_clustered_percent),
    #          alpha = .3)


ggsave(filename = "plots/fig4c.png", plot = fig4c, device = "png", dpi = 200, height = 7, width = 5)

#All 12 that have increased point estimates when weighted use year + county FEs
outliers <-
    dumbbell_data %>% 
    dplyr::select(model_number, model_rank, weight, point_est_percent) %>% 
    pivot_wider(id_cols = model_number:model_rank, names_from = weight, values_from = point_est_percent) %>% 
    filter(Weighted > Unweighted) %>% 
    left_join(
        grid,
        by=c("model_number")
    )
print(outliers)

# Create Fig 4a, which deals with a different type of weighting in whether PM2.5 was 
# aggregated to the county level using population or area weighting.
fig4a <-
    final %>%
    ggplot(aes(x = areaw_mean_pm2.5, y = popw_mean_pm2.5)) +
    geom_point(color = "blue4", alpha = .2) +
    geom_abline(slope = 1, intercept = 0, color = "black") +
    theme_minimal() +
    xlab("Area-weighted mean smoke PM2.5 (µg/m3)") +
    ylab("Population-weighted mean smoke PM2.5 (µg/m3)") +
    geom_text(
        label = paste0(
            "R = ", 
            cor(final$areaw_mean_pm2.5, final$popw_mean_pm2.5) %>% round(digits = 3)
        ),
        x = 1, y = 65
    )
    
ggsave(filename = "plots/fig4a.png", plot = fig4a, device = "png", dpi = 200, height = 5, width = 7)




