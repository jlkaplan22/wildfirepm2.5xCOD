#### TWFEs with varying model specifications ####
#Comparing area-weighted smoke PM2.5 to population-weighted smoke PM2.5--
#note that is is in terms of aggregating to the county level

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
    dplyr::select(model_number, x, fes, controls) %>% 
    mutate(
        x_pop = x,
        x_area = str_replace(x, "pop", "area")
    )

output <- as.data.frame(matrix(nrow=nrow(grid)*2, ncol=9))
colnames(output) <- c("model_number", "weight", "mean_pm2.5", 
                      "se_county_cluster", "pval_county_cluster",
                      "se_iid", "pval_iid",
                      "se_hetero", "pval_hetero")

tic()
for (i in 1:nrow(grid)) {
    x_areaw <- grid$x_area[i]
    x_popw <- grid$x_pop[i]
    fes <- grid$fes[i]
    controls <- grid$controls[i]
    
    model_output_areaweight <- modeler(final, x_areaw, controls, fes, final$fipsihme)
    model_output_popweight <- modeler(final, x_popw, controls, fes, final$fipsihme)
    
    output[i*2 - 1,] <- 
        c(
            grid$model_number[i],
            "Area",
            model_output_areaweight
        )
    output[i*2,] <- 
        c(
            grid$model_number[i],
            "Population",
            model_output_popweight
        )
}
toc()

fig4b_mod_outputs <-
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
        
        preferred_mod = ifelse(model_number==26 & weight=="Population", 1, 0)
    ) %>% 
    arrange(point_est) %>% 
    cbind(
        seq(1:nrow(.))
    ) %>% 
    rename(model_rank = `seq(1:nrow(.))`)

# Trying to create dumbbell plot:
area_mods <-
    fig4b_mod_outputs %>% 
    filter(weight == "Area") %>% 
    dplyr::select(-model_rank) %>% 
    arrange(point_est) %>% 
    cbind(
        seq(1:nrow(.))
    ) %>% 
    rename(model_rank = `seq(1:nrow(.))`)

pop_mods <-
    fig4b_mod_outputs %>% 
    filter(weight == "Population") %>% 
    dplyr::select(-model_rank) %>% 
    left_join(
        area_mods %>% dplyr::select(model_number, model_rank)
    ) %>% 
    arrange(model_rank)

dumbbell_data <-
    rbind(area_mods, pop_mods)
    

fig4b <-
    dumbbell_data %>% 
    left_join(grid %>% select(model_number, x), by = "model_number") %>% 
    mutate(
        x = 
            case_when(
                x == "popw_mean_pm2.5" ~ "Monthly mean PM2.5",
                x == "popw_daysover0" ~ "Monthly days over 0µg/m3",
                x == "popw_daysover5" ~ "Monthly days over 5µg/m3"
            )
    ) %>% 
    ggplot() +
    # geom_segment(data = area_mods,
    #              aes(x = point_est_percent, y = model_rank,
    #                  xend = pop_mods$point_est_percent, yend = pop_mods$model_rank),
    #              color = "#aeb6bf", alpha = .7, size = 1) +
    
    # scale_x_break(c(-1.2, -1.9)) +
    # scale_x_break(c(-.7, -1.1)) +
    # scale_x_break(c(-.2, -.5)) +
    
    facet_wrap(vars(x), scales = "free") +
    
    geom_point(aes(x = point_est_percent, y = model_rank, color = weight), size = 1, show.legend = TRUE) +
    geom_vline(xintercept = 0, colour = DEFAULT_COLOR, lty = 2, size=0.25) + 
    theme_minimal() +
    scale_color_brewer(palette = "Set2") +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
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
    
ggsave(filename = "plots/fig4b.png", plot = fig4b, device = "png", dpi = 200, height = 7, width = 10)


