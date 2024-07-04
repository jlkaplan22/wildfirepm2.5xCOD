#### TWFEs with varying model specifications ####
#All-cause mortality and mean smoke PM2.5

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

output <- as.data.frame(matrix(nrow=nrow(grid), ncol=8))
colnames(output) <- 
    c(
        "model_number", "popw_mean_pm2.5", 
        "se_county_cluster", "pval_county_cluster",
        "se_iid", "pval_iid",
        "se_hetero", "pval_hetero"
    )

for (i in 1:nrow(grid)) {
    x <- grid$x[i]
    fes <- grid$fes[i]
    controls <- grid$controls[i]
    
    model_output <- modeler(final, x, controls, fes, final$fipsihme)
    
    output[i,] <- 
        c(
            grid$model_number[i],
            model_output
        )
}

fig2_mod_outputs <-
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
        
        preferred_mod = ifelse(model_number==26, 1, 0)
    ) %>% 
    arrange(point_est) %>% 
    cbind(
        seq(1:nrow(.))
    ) %>% 
    rename(model_rank = `seq(1:nrow(.))`)

# Specific values useful for writing results section
fig2_mod_outputs %>% filter(point_est_percent > 0) %>% nrow() #39
fig2_mod_outputs %>% filter(point_est_percent < 0) %>% nrow() #111
fig2_mod_outputs %>% filter(pval_county_cluster < .05) %>% nrow() #32
fig2_mod_outputs %>% filter(pval_iid < .05) %>% nrow() #120

fig2_mod_outputs$point_est_percent %>% max() #0.195171
fig2_mod_outputs$point_est_percent %>% min() #-1.949829
fig2_mod_outputs %>% filter(point_est_percent > -.5) %>% pull(point_est_percent) %>% min() #-0.124697

fig2_mod_outputs %>% 
    left_join(grid, by = "model_number") %>% 
    group_by(x) %>% 
    summarize(
        median_point_est = median(point_est_percent),
        
        median_CIrange_countycluster = median(CI_upper_clustered_percent - CI_lower_clustered_percent),
        median_CIrange_iid = median(CI_upper_iid_percent - CI_lower_iid_percent)
    )

fig2_mod_outputs %>% 
    left_join(grid, by = "model_number") %>% 
    group_by(fes) %>% 
    summarize(
        median_point_est = median(point_est_percent),
        
        median_CIrange_countycluster = median(CI_upper_clustered_percent - CI_lower_clustered_percent),
        median_CIrange_iid = median(CI_upper_iid_percent - CI_lower_iid_percent)
    )

# Create figure
modspecs_x <-
    grid %>% 
    left_join(
        fig2_mod_outputs %>% dplyr::select(model_number, model_rank),
        by=c("model_number")
    ) %>% 
    mutate(
        x_name =
            case_when(
                x == "popw_mean_pm2.5" ~ "Monthly mean",
                x == "popw_daysover0" ~ "Days >0mcg/m3",
                x == "popw_daysover5" ~ "Days >5mcg/m3",

                TRUE ~ "error"
            )
    )

modspecs_fes <-
    grid %>% 
    left_join(
        fig2_mod_outputs %>% dplyr::select(model_number, model_rank),
        by=c("model_number")
    ) %>% 
    mutate(
        fe_name =
            case_when(
                fes == "fipsihme + year" ~ "County + Year",
                fes == "fipsihme + yearmonth" ~ "County + Year-Month",
                fes == "fipsihme^month + year" ~ "County*Month + Year",
                fes == "fipsihme + yearmonth + statefips^month" ~ "County + Year-Month + State*Month",
                fes == "fipsihme + yearmonth + statefips^year" ~ "County + Year-Month + State*Year",
                
                TRUE ~ "error"
            )
    )

modspecs_temp <-
    grid %>% 
    left_join(
        fig2_mod_outputs %>% dplyr::select(model_number, model_rank),
        by=c("model_number")
    ) %>% 
    mutate(
        temp_name =
            case_when(
                str_detect(controls, "df=5") ~ "NS, df=5",
                str_detect(controls, "df=3") ~ "NS, df=3",
                str_detect(controls, "popw_monthly_mean_tempF") ~ "Linear",
                str_detect(controls, "areaw_monthly_mean_tempF") ~ "Linear, area-weighted",
                str_detect(controls, "popw_days_70to80") ~ "10F Binned",
                TRUE ~ "None"
            )
    ) %>% 
    arrange(model_rank)

modspecs_precip <-
    grid %>% 
    left_join(
        fig2_mod_outputs %>% dplyr::select(model_number, model_rank),
        by=c("model_number")
    ) %>% 
    mutate(
        precip_name =
            case_when(
                #str_detect(controls, "df=5)") ~ "NS, df=5",
                #str_detect(controls, "df=3") ~ "NS, df=3",
                str_detect(controls, "popw_monthly_mean_precip") ~ "Linear",
                #str_detect(controls, "areaw_total_precip") ~ "Linear, area-weighted",
                TRUE ~ "None"
            )
    ) %>% 
    mutate(
        precip_name = 
            factor(
                precip_name,
                levels = c("Linear", "None", "Precipitation")
            )
    ) %>% 
    arrange(model_rank)

#Create figure 2a
low_coef_models <- 
    fig2_mod_outputs %>% 
    filter(point_est_percent < -1.8) %>% 
    pull(model_number)

create_fig2_coefs <- function(exclude_low_coefs) {
    fig2_mod_outputs %>% 
        #filter(model_number %in% low_coef_models != exclude_low_coefs) %>% 
        ggplot(aes(x = model_rank, y = point_est_percent)) + 
        geom_linerange(aes(ymin = CI_lower_clustered_percent, ymax = CI_upper_clustered_percent), lwd=.2, color = "gray30") +
        #geom_linerange(aes(ymin = CI_lower_iid_percent, ymax = CI_upper_iid_percent), lwd=.6, color = "dodgerblue3") +
        geom_point(size=.5) +
        theme_minimal() +
        geom_hline(yintercept = 0, colour = DEFAULT_COLOR, lty = 2, size=0.25) + 
        ylab("% Change in Mortality Rate") +
        
        theme(
            panel.grid = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 18),
            plot.margin = unit(c(0,1,0,1), "cm"),
            legend.position = "none"
        )
}

size <- .8
#height_2a <- .4
#height_2b <- .4
#width_2a <- .5
#width_2b <- .1

create_fig2_x_grid <- function(exclude_low_coefs) {
    modspecs_x %>% 
        #filter(model_number %in% low_coef_models != exclude_low_coefs) %>% 
        ggplot(aes(x = model_rank, y = as.factor(x_name))) + 
        geom_tile(fill = "red2") +
        scale_alpha_manual(NULL, values = c(0, 0, 1, .4)) +
        theme_minimal() + 
        ylab("Smoke PM2.5") +
        theme(
            legend.position = "none",
            legend.text = element_text(size = 12),
            panel.border = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 8, angle=0, face = "bold"),
            axis.text.y = element_text(size = 8, angle=0),
            panel.grid = element_blank(),
            plot.margin = unit(c(0,1,0,1), "cm")
        ) +
        coord_equal()
}

create_fig2_fe_grid <- function(exclude_low_coefs) {
    modspecs_fes %>% 
        #filter(model_number %in% low_coef_models != exclude_low_coefs) %>% 
        ggplot(aes(x = model_rank, y = as.factor(fe_name))) + 
        geom_tile(fill = "darkgray") +
        scale_alpha_manual(NULL, values = c(0, 0, 1, .4)) +
        theme_minimal() + 
        ylab("Fixed Effects") +
        theme(
            legend.position = "none",
            legend.text = element_text(size = 12),
            panel.border = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 8, angle=0, face = "bold"),
            axis.text.y = element_text(size = 8, angle=0),
            panel.grid = element_blank(),
            plot.margin = unit(c(0,1,0,1), "cm")
        ) +
        coord_equal()
}

create_fig2_temp_grid <- function(exclude_low_coefs) {
    modspecs_temp %>% 
        #filter(model_number %in% low_coef_models != exclude_low_coefs) %>% 
        ggplot(aes(x = model_rank, y = as.factor(temp_name))) + 
        geom_tile(fill = "maroon") +
        scale_alpha_manual(NULL, values = c(0, 0, 1, .4)) +
        theme_minimal() + 
        ylab("Temperature") +
        theme(
            panel.border = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 8, angle=0, face = "bold"),
            panel.grid = element_blank(),
            plot.margin = unit(c(0,1,0,1), "cm"),
            legend.position = "none"
        ) +
        coord_equal()
    
}

create_fig2_precip_grid <- function(exclude_low_coefs) {
    modspecs_precip %>% 
        #filter(model_number %in% low_coef_models != exclude_low_coefs) %>% 
        ggplot(aes(x = model_rank, y = as.factor(precip_name))) + 
        geom_tile(fill = "royalblue") +
        scale_fill_manual(values=colorpalette) +
        scale_alpha_manual(NULL, values = c(0, 0, 1, .4)) +
        theme_minimal() + 
        #ggtitle("Precipitation")+
        ylab("Precipitation") +
        theme(
            plot.title = element_text(size = 8, hjust = 0),
            panel.border = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 8, angle = 0, face = "bold"),
            panel.grid = element_blank(),
            plot.margin = unit(c(0,1,0,1), "cm"),
            legend.position = "bottom",
            legend.key.size = unit(.2, "cm"),
            legend.text = element_text(size=12),
            legend.title = element_blank()
        ) +
        coord_equal()
    
}


coefs <- 
    create_fig2_coefs(exclude_low_coefs = TRUE)
    #scale_y_break(c(-0.25, -.45)) +
    #scale_y_break(c(-0.65, -1))
    
model_x <- create_fig2_x_grid(exclude_low_coefs = TRUE)
model_fes <- create_fig2_fe_grid(exclude_low_coefs = TRUE)
model_temp <- create_fig2_temp_grid(exclude_low_coefs = TRUE)
model_precip <- create_fig2_precip_grid(exclude_low_coefs = TRUE)

fig2a <- coefs + model_x + model_fes + model_temp + model_precip + plot_layout(ncol = 1) # heights = c(20,4,1.5,1.5) was taken out

ggsave(filename = "plots/fig2a_multiple_x.png", plot = fig2a, device = "png", dpi = 200, height = 15, width = 20)

#Fig 2b:
coefs <- create_fig2_coefs(exclude_low_coefs = FALSE)
model_fes <- create_fig2_fe_grid(exclude_low_coefs = FALSE)
model_temp <- create_fig2_temp_grid(exclude_low_coefs = FALSE)
model_precip <- create_fig2_precip_grid(exclude_low_coefs = FALSE)

fig2b <- coefs + model_fes + model_temp + model_precip + plot_layout(ncol = 1)

#ggsave(filename = "plots/fig2b_daysover0.png", plot = fig2b, device = "png", dpi = 200, height = 5, width = 3)
