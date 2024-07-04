temp_fig2_grapher <- function(fes) {
    
    model_name <-
            case_when(
                fes == "fipsihme + year" ~ "County + Year",
                fes == "fipsihme + yearmonth" ~ "County + Year-Month",
                fes == "fipsihme + yearmonth + statefips^year" ~ "County + Year-Month + State*Year",
                fes == "fipsihme^month + year" ~ "County*Month + Year",
                fes == "fipsihme + yearmonth + statefips^month" ~ "County + Year-Month + State*Month",
                TRUE ~ ""
            )
        
    temp <- 
        c("", 
          "areaw_monthly_mean_tempF", 
          "popw_monthly_mean_tempF", 
          
          "ns(areaw_monthly_mean_tempF, df=3)", 
          "ns(popw_monthly_mean_tempF, df=3)", 

          #Binned analysis
          "areaw_days_u30 + areaw_days_30to40 + areaw_days_50to60 + areaw_days_60to70 +
          areaw_days_70to80 + areaw_days_80to90 + areaw_days_90to100 + areaw_days_o100",
          
          "popw_days_u30 + popw_days_30to40 + popw_days_50to60 + popw_days_60to70 +
          popw_days_70to80 + popw_days_80to90 + popw_days_90to100 + popw_days_o100"
          )
    
    precip <- 
        c("")
    
    
    grid <- 
        expand.grid(fes = fes, temp = temp, precip = precip) %>% 
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
        dplyr::select(model_number, fes, controls)
    
    output <- as.data.frame(matrix(nrow=nrow(grid), ncol=8))
    colnames(output) <- 
        c(
            "model_number", "popw_mean_pm2.5", 
            "se_county_cluster", "pval_county_cluster",
            "se_iid", "pval_iid",
            "se_hetero", "pval_hetero"
        )
    
    for (i in 1:nrow(grid)) {
        fes <- grid$fes[i]
        controls <- grid$controls[i]
        
        model_output <- modeler(final, "popw_mean_pm2.5", controls, fes, final$fipsihme)
        
        output[i,] <- 
            c(
                grid$model_number[i],
                model_output
            )
    }

    fig2_temp_mod_outputs <-
        output %>% 
        mutate(
            model_number = model_number,
            #exponentiate
            point_est = exp(popw_mean_pm2.5),
            CI_lower_clustered = exp(popw_mean_pm2.5 - qnorm(0.975) * se_county_cluster),
            CI_upper_clustered = exp(popw_mean_pm2.5 + qnorm(0.975) * se_county_cluster),
            CI_lower_iid = exp(popw_mean_pm2.5 - qnorm(0.975) * se_iid),
            CI_upper_iid = exp(popw_mean_pm2.5 + qnorm(0.975) * se_iid),
            
            #convert to interpretable percentages
            point_est_percent = (point_est - 1) * 100,
            CI_lower_clustered_percent = (CI_lower_clustered - 1) * 100,
            CI_upper_clustered_percent = (CI_upper_clustered - 1) * 100,
            CI_lower_iid_percent = (CI_lower_iid - 1) * 100,
            CI_upper_iid_percent = (CI_upper_iid - 1) * 100,
            
        ) %>% 
        arrange(point_est) %>% 
        cbind(
            seq(1:nrow(.))
        ) %>% 
        rename(model_rank = `seq(1:nrow(.))`) %>% 
        left_join(
            grid %>% 
                mutate(
                    model_name = 
                        case_when(
                            controls == "" ~ "No temperature control",
                            controls == "popw_monthly_mean_tempF" ~ "Monthly mean temperature (population-weighted)",
                            controls == "areaw_monthly_mean_tempF" ~ "Monthly mean temperature (area-weighted)",
                            controls == "ns(popw_monthly_mean_tempF, df=3)" ~ "NS, df=3, population-weight",
                            controls == "ns(areaw_monthly_mean_tempF, df=3)" ~ "NS, df=3, area-weight",
                            str_detect(controls, "popw_days_60to70") ~ "10F Bins (population-weighted)",
                            str_detect(controls, "areaw_days_60to70") ~ "10F Bins (area-weighted)",
                            TRUE ~ ""
                        )
                ),
            by = "model_number"
        )
    
    # Create the forest plot
    plot <-
        fig2_temp_mod_outputs %>% 
        ggplot(aes(x = point_est_percent, y = model_name)) +
        
        geom_point(size = 1) +
        geom_linerange(aes(xmin = CI_lower_clustered_percent, xmax = CI_upper_clustered_percent), lwd=.2, color = "gray30") +
        #geom_linerange(aes(xmin = CI_lower_iid_percent, xmax = CI_upper_iid_percent), lwd=.6, color = "dodgerblue3") +
        geom_vline(xintercept = 0, colour = DEFAULT_COLOR, lty = 2, size=0.25) + 
        
        ggtitle(paste("Model FEs: ", model_name)) +
        xlab("% Change in Mortality Rate") +
        theme_minimal() +
        theme(
            panel.grid = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_text(size = 5),
            
            strip.text = element_text(face = "bold"),
            plot.title = element_text(hjust = 0.5)
        )
    
    return(plot)
}

precip_fig2_grapher <- function(fes) {
    model_name <-
        case_when(
            fes == "fipsihme + year" ~ "County + Year",
            fes == "fipsihme + yearmonth" ~ "County + Year-Month",
            fes == "fipsihme + yearmonth + statefips^year" ~ "County + Year-Month + State*Year",
            fes == "fipsihme^month + year" ~ "County*Month + Year",
            fes == "fipsihme + yearmonth + statefips^month" ~ "County + Year-Month + State*Month",
            TRUE ~ ""
        )
    
    temp <- 
        c("")
    
    precip <- 
        c("", 
          "areaw_monthly_mean_precip", 
          "popw_monthly_mean_precip", 
          
          "areaw_days_w_precip", 
          "popw_days_w_precip",
          
          "areaw_days_above_30mm", 
          "popw_days_above_30mm",
          
          "ns(popw_monthly_mean_precip, df=3)", 
          "ns(popw_monthly_mean_precip, df=5)")
    
    
    grid <- 
        expand.grid(fes = fes, temp = temp, precip = precip) %>% 
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
        dplyr::select(model_number, fes, controls)
    
    output <- as.data.frame(matrix(nrow=nrow(grid), ncol=8))
    colnames(output) <- 
        c(
            "model_number", "popw_mean_pm2.5", 
            "se_county_cluster", "pval_county_cluster",
            "se_iid", "pval_iid",
            "se_hetero", "pval_hetero"
        )
    
    for (i in 1:nrow(grid)) {
        fes <- grid$fes[i]
        controls <- grid$controls[i]
        
        model_output <- modeler(final, "popw_mean_pm2.5", controls, fes, final$fipsihme)
        
        output[i,] <- 
            c(
                grid$model_number[i],
                model_output
            )
    }

    fig2_precip_mod_outputs <-
        output %>% 
        mutate(
            model_number = model_number,
            #exponentiate
            point_est = exp(popw_mean_pm2.5),
            CI_lower_clustered = exp(popw_mean_pm2.5 - qnorm(0.975) * se_county_cluster),
            CI_upper_clustered = exp(popw_mean_pm2.5 + qnorm(0.975) * se_county_cluster),
            CI_lower_iid = exp(popw_mean_pm2.5 - qnorm(0.975) * se_iid),
            CI_upper_iid = exp(popw_mean_pm2.5 + qnorm(0.975) * se_iid),
            
            #convert to interpretable percentages
            point_est_percent = (point_est - 1) * 100,
            CI_lower_clustered_percent = (CI_lower_clustered - 1) * 100,
            CI_upper_clustered_percent = (CI_upper_clustered - 1) * 100,
            CI_lower_iid_percent = (CI_lower_iid - 1) * 100,
            CI_upper_iid_percent = (CI_upper_iid - 1) * 100,
            
            #preferred_mod = ifelse(model_number==26, 1, 0)
        ) %>% 
        arrange(point_est) %>% 
        cbind(
            seq(1:nrow(.))
        ) %>% 
        rename(model_rank = `seq(1:nrow(.))`) %>% 
        left_join(
            grid %>% 
                mutate(
                    model_name = 
                        case_when(
                            controls == "" ~ "No precipitation control",
                            controls == "popw_monthly_mean_precip" ~ "Monthly mean precipitation (population-weighted)",
                            controls == "ns(popw_monthly_mean_precip, df=3)" ~ "NS(Monthly mean precipitation, n = 3)",
                            controls == "ns(popw_monthly_mean_precip, df=5)" ~ "NS(Monthly mean precipitation, n = 5)",
                            controls == "popw_days_w_precip" ~ "Monthly days with precipitation (population-weighted)",
                            controls == "popw_days_above_30mm" ~ "Monthly days with precipitation >30mm (population-weighted)",
                            
                            
                            controls == "areaw_monthly_mean_precip" ~ "Monthly mean precipitation (area-weighted)",
                            controls == "areaw_days_w_precip" ~ "Monthly days with precipitation (area-weighted)",
                            controls == "areaw_days_above_30mm" ~ "Monthly days with precipitation >30mm (area-weighted)",
                            
                            TRUE ~ ""
                        )
                ),
            by = "model_number"
        )
    
    # Create the forest plot
    plot <-
        fig2_precip_mod_outputs %>% 
        ggplot(aes(x = point_est_percent, y = model_name)) +
        
        geom_point(size = 1) +
        geom_linerange(aes(xmin = CI_lower_clustered_percent, xmax = CI_upper_clustered_percent), lwd=.2, color = "gray30") +
        #geom_linerange(aes(xmin = CI_lower_iid_percent, xmax = CI_upper_iid_percent), lwd=.6, color = "dodgerblue3") +
        geom_vline(xintercept = 0, colour = DEFAULT_COLOR, lty = 2, size=0.25) + 
        
        xlab("% Change in Mortality Rate") +
        ggtitle(paste("Model FEs: ", model_name)) +
        theme_minimal() +
        theme(
            panel.grid = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_text(size = 5),
            
            strip.text = element_text(face = "bold"),
            plot.title = element_text(hjust = 0.5)
        )
    
    return(plot)
}

precip1 <- precip_fig2_grapher(fes = c("fipsihme + yearmonth")) + coord_cartesian(xlim = c(-.25, .1))
precip2 <- precip_fig2_grapher(fes = c("fipsihme + yearmonth + statefips^month")) + coord_cartesian(xlim = c(-.25, .1))
precip3 <- precip_fig2_grapher(fes = c("fipsihme + yearmonth + statefips^year")) + coord_cartesian(xlim = c(-.25, .1))
precip4 <- precip_fig2_grapher(fes = c("fipsihme^month + year")) + coord_cartesian(xlim = c(-.25, .1))
precip5 <- precip_fig2_grapher(fes = c("fipsihme + year"))

fig2_precips <- precip1 / precip2 / precip3 / precip4 / precip5 + plot_layout(ncol = 1)
ggsave(filename = "plots/fig2b_varying_precips.png", plot = fig2_precips, device = "png", dpi = 200, height = 6, width = 8)



temp1 <- temp_fig2_grapher(fes = c("fipsihme + yearmonth")) + coord_cartesian(xlim = c(-.25, .15))
temp2 <- temp_fig2_grapher(fes = c("fipsihme + yearmonth + statefips^month")) + coord_cartesian(xlim = c(-.25, .15))
temp3 <- temp_fig2_grapher(fes = c("fipsihme + yearmonth + statefips^year")) + coord_cartesian(xlim = c(-.25, .15))
temp4 <- temp_fig2_grapher(fes = c("fipsihme^month + year")) + coord_cartesian(xlim = c(-.25, .15))
temp5 <- temp_fig2_grapher(fes = c("fipsihme + year"))

fig2_temps <- temp1 / temp2 / temp3 / temp4 / temp5 + plot_layout(ncol = 1)
ggsave(filename = "plots/fig2b_varying_temps.png", plot = fig2_temps, device = "png", dpi = 200, height = 6, width = 8)
