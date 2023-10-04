#### TWFEs with varying model specifications ####
#All-cause mortality and mean smoke PM2.5

fes <- c("fipsihme + yearmonth", "fipsihme^month + year", "fipsihme + year")
temp <- c("", "weighted_temp", "temp", "ns(weighted_temp, df=3)", "ns(weighted_temp, df=5)")
precip <- c("", "weighted_precip", "precip", "ns(weighted_precip, df=3)", "ns(weighted_precip, df=5)")

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
colnames(output) <- c("model_number", "mean_pm2.5", 
                      "se_county_cluster", "pval_county_cluster",
                      "se_iid", "pval_iid",
                      "se_hetero", "pval_hetero")

tic() #takes about 91s
for (i in 1:nrow(grid)) {
    fes <- grid$fes[i]
    controls <- grid$controls[i]
    
    model_output <- modeler(final, "mean_pm2.5", controls, fes, final$fipsihme)
    
    output[i,] <- 
        c(
            grid$model_number[i],
            model_output
        )
}
toc()

fig2_mod_outputs <-
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
        
        preferred_mod = ifelse(model_number==26, 1, 0)
    ) %>% 
    arrange(point_est) %>% 
    cbind(
        seq(1:nrow(.))
    ) %>% 
    rename(model_rank = `seq(1:nrow(.))`)

#Number of models that fit X specification (for writing results section):
fig2_mod_outputs %>% 
    filter(pval_iid < .05) %>% 
    nrow() #59

fig2_mod_outputs %>% 
    filter(pval_county_cluster < .05) %>% 
    nrow() #8

fig2_mod_outputs %>% 
    filter(mean_pm2.5 < 0) %>% 
    nrow() #56

fig2_mod_outputs %>% 
    filter(mean_pm2.5 > 0) %>% 
    nrow() #19

fig2_mod_outputs %>% 
    left_join(grid, by=c("model_number")) %>% 
    filter(fes == "fipsihme + yearmonth") %>% 
    filter(mean_pm2.5 > 0) %>% 
    nrow() #4

fig2_mod_outputs %>% 
    left_join(grid, by=c("model_number")) %>% 
    filter(fes == "fipsihme + yearmonth") %>% 
    filter(mean_pm2.5 < 0) %>% 
    nrow() #21

fig2_mod_outputs %>% 
    left_join(grid, by=c("model_number")) %>% 
    filter(fes == "fipsihme + year") %>% 
    filter(mean_pm2.5 > 0) %>% 
    nrow() #15

fig2_mod_outputs %>% 
    left_join(grid, by=c("model_number")) %>% 
    filter(fes == "fipsihme + year") %>% 
    filter(mean_pm2.5 < 0) %>% 
    nrow() #10

#Create color-coded legend for FEs
modspecs_fes <-
    grid %>% 
    left_join(
        fig2_mod_outputs %>% dplyr::select(model_number, model_rank),
        by=c("model_number")
    ) %>% 
    mutate(
        model_number = model_number,
        model_rank = model_rank,
        `fipsihme^month` = ifelse(str_detect(fes, "fipsihme\\^month"), 1, 0),
        fipsihme = ifelse(str_detect(fes, "fipsihme") & `fipsihme^month` == 0, 1, 0),
        yearmonth = ifelse(str_detect(fes, "yearmonth"), 1, 0),
        year = ifelse(str_detect(fes, "year") & yearmonth == 0, 1, 0),
        
        fe_combo =
            case_when(
                `fipsihme^month` == 1 & year == 1 ~ "countymonth+year",
                fipsihme == 1 & year == 1 ~ "county+year",
                fipsihme == 1 & yearmonth == 1 ~ "county+yearmonth"
            ),
        
        .keep = c("none")
    ) %>% 
    pivot_longer(`fipsihme^month`:year, names_to="feature", values_to="value") %>% 
    mutate(
        feature = 
            factor(feature,
                   levels =
                       c(
                           "fipsihme", "fipsihme^month", "yearmonth", "year"
                       )
            ),
        
        color = ifelse(interaction(fe_combo, value) %>% str_detect("0"), 0, interaction(fe_combo, value)) %>% as.factor(),
    )

modspecs_temp <-
    grid %>% 
    left_join(
        fig2_mod_outputs %>% dplyr::select(model_number, model_rank),
        by=c("model_number")
    ) %>% 
    mutate(
        model_number = model_number,
        model_rank = model_rank,
        
        dummy = "1",
        
        temp_choice =
            case_when(
                str_detect(controls, "ns\\(weighted_temp, df=5\\)") ~ "NS, df=5",
                str_detect(controls, "ns\\(weighted_temp, df=3\\)") ~ "NS, df=3",
                str_detect(controls, "weighted_temp") ~ "Linear",
                str_detect(controls, "temp") ~ "Linear, unweighted",
                TRUE ~ "None"
            ) %>% 
            factor(
                levels = 
                    c(
                        "None", "Linear, unweighted", "Linear", "NS, df=3", "NS, df=5"
                    )
            ),

        .keep = c("none")
    ) %>% 
    arrange(model_rank)

modspecs_precip <-
    grid %>% 
    left_join(
        fig2_mod_outputs %>% dplyr::select(model_number, model_rank),
        by=c("model_number")
    ) %>% 
    mutate(
        model_number = model_number,
        model_rank = model_rank,
        
        dummy = "1",
        
        precip_choice =
            case_when(
                str_detect(controls, "ns\\(weighted_precip, df=5\\)") ~ "NS, df=5",
                str_detect(controls, "ns\\(weighted_precip, df=3\\)") ~ "NS, df=3",
                str_detect(controls, "weighted_precip") ~ "Linear",
                str_detect(controls, "precip") ~ "Linear, unweighted",
                TRUE ~ "None"
            ) %>% 
            factor(
                levels = 
                    c(
                        "None", "Linear, unweighted", "Linear", "NS, df=3", "NS, df=5"
                    )
            ),
        
        .keep = c("none")
    ) %>% 
    arrange(model_rank)

#Create figure 2a
low_coef_models <- c(3, 18, 33, 48, 63)

coefs <-
    fig2_mod_outputs %>% 
    filter(model_number %in% low_coef_models != TRUE) %>% 
    ggplot(aes(x = model_rank, y = point_est_percent)) + 
    geom_linerange(aes(ymin = CI_lower_clustered_percent, ymax = CI_upper_clustered_percent), lwd=.2, color = "gray30") +
    geom_linerange(aes(ymin = CI_lower_iid_percent, ymax = CI_upper_iid_percent), lwd=.6, color = "dodgerblue3") +
    geom_point(size=.5) +
    geom_point(data = fig2_mod_outputs %>% filter(preferred_mod==1),
               aes(x=model_rank, y = point_est_percent),
               color = "red", size = 1) +
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
    ) +
    #Spotlight the prefered model
    annotate("rect",
             xmin = fig2_mod_outputs %>% filter(preferred_mod==1) %>% pull(model_rank) - .5,
             xmax = fig2_mod_outputs %>% filter(preferred_mod==1) %>% pull(model_rank) + .5,
             ymin = fig2_mod_outputs %>% filter(preferred_mod==1) %>% pull(CI_lower_clustered_percent) - .05,
             ymax = fig2_mod_outputs %>% filter(preferred_mod==1) %>% pull(CI_upper_clustered_percent) + .05,
               alpha = .2)


size <- .8
#height_2a <- .4
#height_2b <- .4
#width_2a <- .5
#width_2b <- .1
colorpalette <- c("white", "#008B45", "#BA1921", "dodgerblue1", "#5E569B")

model_fes <-
    modspecs_fes %>% 
    filter(model_number %in% low_coef_models != TRUE) %>% 
    ggplot(aes(x = model_rank, y = as.factor(feature), fill = color)) + 
    geom_tile(color = "gray") +
    scale_alpha_manual(NULL, values = c(0, 0, 1, .4)) +
    scale_y_discrete(labels = c("County", "County*Cal. Month", "Year-Month", "Year")) +
    theme_minimal() + 
    scale_fill_manual(values=c("white", "#C5ADCD", "#F8ACAA", "#AFCFD0")) +
    ylab("FEs") +
    theme(
        legend.position = "none",
        legend.text = element_text(size = 12),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, angle=0),
        axis.text.y = element_text(size = 10, angle=0),
        panel.grid = element_blank(),
        plot.margin = unit(c(0,1,0,1), "cm")
    ) +
    coord_equal()

model_temp <-
    modspecs_temp %>% 
    filter(model_number %in% low_coef_models != TRUE) %>% 
    ggplot(aes(x = model_rank, y = dummy, fill = temp_choice)) + 
    geom_tile(color = "gray") +
    scale_fill_manual(values=colorpalette) +
    scale_alpha_manual(NULL, values = c(0, 0, 1, .4)) +
    theme_minimal() + 
    ylab("Temp") +
    theme(
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, angle=0),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(0,1,0,1), "cm"),
        legend.position = "none"
    ) +
    coord_equal()

model_precip <-
    modspecs_precip %>% 
    filter(model_number %in% low_coef_models != TRUE) %>% 
    ggplot(aes(x = model_rank, y = dummy, fill = precip_choice)) + 
    geom_tile(color = "gray") +
    scale_fill_manual(values=colorpalette) +
    scale_alpha_manual(NULL, values = c(0, 0, 1, .4)) +
    theme_minimal() + 
    ylab("Precip") +
    theme(
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, angle=0),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(0,1,0,1), "cm"),
        legend.position = "bottom",
        legend.key.size = unit(.2, "cm"),
        legend.text = element_text(size=12),
        legend.title = element_blank()
    ) +
    coord_equal()

fig2a <- coefs + model_fes + model_temp + model_precip + plot_layout(ncol = 1) # heights = c(20,4,1.5,1.5) was taken out

ggsave(filename = "plots/fig2a.png", plot = fig2a, device = "png", dpi = 200, height = 8, width = 10)

#Fig 2b:
coefs <-
    fig2_mod_outputs %>% 
    filter(model_number %in% low_coef_models) %>% 
    ggplot(aes(x = model_rank, y = point_est_percent)) + 
    geom_linerange(aes(ymin = CI_lower_clustered_percent, ymax = CI_upper_clustered_percent), lwd=.2, color = "gray30") +
    geom_linerange(aes(ymin = CI_lower_iid_percent, ymax = CI_upper_iid_percent), lwd=.8, color = "dodgerblue3") +
    geom_point(size=.5) +
    theme_minimal() +
    ylab("% Change in Mortality Rate") +
    theme(
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        plot.margin = unit(c(0,1,0,1), "cm"),
        legend.position = "none"
    )
    

model_fes <-
    modspecs_fes %>% 
    filter(model_number %in% low_coef_models) %>% 
    ggplot(aes(x = model_rank, y = as.factor(feature), fill = color)) + 
    geom_tile(color = "gray") +
    scale_alpha_manual(NULL, values = c(0, 0, 1, .4)) +
    scale_y_discrete(labels = c("County", "County*Cal. Month", "Year-Month", "Year")) +
    theme_minimal() + 
    #scale_fill_gradient(low = "white", high = "black") +
    scale_fill_manual(values=c("white", "#C5ADCD", "#F8ACAA", "#AFCFD0")) +
    ylab("FEs") +
    theme(
        legend.position = "none",
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8, angle=0),
        panel.grid = element_blank(),
        plot.margin = unit(c(0,1,0,1), "cm")
    ) +
    coord_equal()

model_temp <-
    modspecs_temp %>% 
    filter(model_number %in% low_coef_models) %>% 
    ggplot(aes(x = model_rank, y = dummy, fill = temp_choice)) + 
    geom_tile(color = "gray") +
    scale_fill_manual(values=colorpalette) +
    scale_alpha_manual(NULL, values = c(0, 0, 1, .4)) +
    theme_minimal() + 
    ylab("Temp") +
    theme(
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8, angle=0),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(0,1,0,1), "cm"),
        legend.position = "none"
    ) +
    coord_equal()

model_precip <-
    modspecs_precip %>% 
    filter(model_number %in% low_coef_models) %>% 
    ggplot(aes(x = model_rank, y = dummy, fill = precip_choice)) + 
    geom_tile(color = "gray") +
    scale_fill_manual(values=colorpalette) +
    scale_alpha_manual(NULL, values = c(0, 0, 1, .4)) +
    theme_minimal() + 
    ylab("Precip") +
    theme(
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8, angle=0),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(0,1,0,1), "cm"),
        legend.position = "none",
        legend.key.size = unit(.2, "cm"),
        legend.text = element_text(size=8),
        legend.title = element_blank()
    ) +
    coord_equal()

fig2b <- coefs + model_fes + model_temp + model_precip + plot_layout(ncol = 1)

ggsave(filename = "plots/fig2b.png", plot = fig2b, device = "png", dpi = 200, height = 5, width = 3)
