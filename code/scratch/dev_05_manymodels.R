#### TWFEs with varying model specifications ####
#All-cause mortality and mean smoke PM2.5

source("code/quickprepfinaldata_AC.R")


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
    cbind(tibble(model_number = seq(1:nrow(grid)))) %>% 
    dplyr::select(model_number, fes, controls)

output <- as.data.frame(matrix(nrow=nrow(grid), ncol=5))
colnames(output) <- c("model_number", "mean_pm2.5", "se_county_cluster", "se_iid", "se_hetero")

tic() #takes about 75s
for (i in 1:nrow(grid)) {
    fes <- grid$fes[i]
    controls <- grid$controls[i]
    
    output[i,] <- 
        c(
            grid$model_number[i],
            modeler(final, "mean_pm2.5", controls, fes, final$fipsihme)
        )
}
toc()

fig2_mod_outputs <-
    output %>% 
    mutate(
        model_number = model_number,
        point_est = exp(mean_pm2.5),
        CI_lower_clustered = exp(mean_pm2.5 - qnorm(0.975) * se_county_cluster),
        CI_upper_clustered = exp(mean_pm2.5 + qnorm(0.975) * se_county_cluster),
        CI_lower_iid = exp(mean_pm2.5 - qnorm(0.975) * se_iid),
        CI_upper_iid = exp(mean_pm2.5 + qnorm(0.975) * se_iid),
        .keep=("none")
    ) %>% 
    arrange(point_est) %>% 
    cbind(
        seq(1:nrow(.))
    ) %>% 
    rename(model_rank = `seq(1:nrow(.))`)

mod_specs <-
    grid %>% 
    left_join(
        fig2_mod_outputs %>% dplyr::select(model_number, model_rank),
        by=c("model_number")
    ) %>% 
    mutate(
        `fipsihme^month` = ifelse(str_detect(fes, "fipsihme\\^month"), 1, 0),
        fipsihme = ifelse(str_detect(fes, "fipsihme") & `fipsihme^month` == 0, 1, 0),
        yearmonth = ifelse(str_detect(fes, "yearmonth"), 1, 0),
        year = ifelse(str_detect(fes, "year") & yearmonth == 0, 1, 0),
        
        `ns(weighted_temp, df=5)` = ifelse(str_detect(controls, "ns\\(weighted_temp, df=5\\)"), 1, 0),
        `ns(weighted_temp, df=3)` = ifelse(str_detect(controls, "ns\\(weighted_temp, df=3\\)"), 1, 0),
        weighted_temp = ifelse(str_detect(controls, "weighted_temp") &
                                              `ns(weighted_temp, df=5)` == 0 &
                                              `ns(weighted_temp, df=3)` == 0, 1, 0),
        temp = ifelse(str_detect(controls, "temp") & 
                          `ns(weighted_temp, df=5)` == 0 &
                          `ns(weighted_temp, df=3)` == 0 & 
                          weighted_temp == 0, 1, 0),
        
        `ns(weighted_precip, df=5)` = ifelse(str_detect(controls, "ns\\(weighted_precip, df=5\\)"), 1, 0),
        `ns(weighted_precip, df=3)` = ifelse(str_detect(controls, "ns\\(weighted_precip, df=3\\)"), 1, 0),
        weighted_precip = ifelse(str_detect(controls, "weighted_precip") &
                                   `ns(weighted_precip, df=5)` == 0 &
                                   `ns(weighted_precip, df=3)` == 0, 1, 0),
        precip = ifelse(str_detect(controls, "precip") & 
                          `ns(weighted_precip, df=5)` == 0 &
                          `ns(weighted_precip, df=3)` == 0 & 
                          weighted_precip == 0, 1, 0),
        
        .keep = c("unused")
    ) %>% 
    pivot_longer(`fipsihme^month`:precip, names_to="feature", values_to="value") %>% 
    mutate(
        feature = 
            factor(feature,
                   levels =
                       c(
                           "fipsihme", "fipsihme^month", "yearmonth", "year",
                           "temp", "weighted_temp", "ns(weighted_temp, df=3)", "ns(weighted_temp, df=5)",
                           "precip", "weighted_precip", "ns(weighted_precip, df=3)", "ns(weighted_precip, df=5)"
                       )
                   )
    )

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
        `ns(weighted_temp, df=5)` = ifelse(str_detect(controls, "ns\\(weighted_temp, df=5\\)"), 1, 0),
        `ns(weighted_temp, df=3)` = ifelse(str_detect(controls, "ns\\(weighted_temp, df=3\\)"), 1, 0),
        weighted_temp = ifelse(str_detect(controls, "weighted_temp") &
                                   `ns(weighted_temp, df=5)` == 0 &
                                   `ns(weighted_temp, df=3)` == 0, 1, 0),
        temp = ifelse(str_detect(controls, "temp") & 
                          `ns(weighted_temp, df=5)` == 0 &
                          `ns(weighted_temp, df=3)` == 0 & 
                          weighted_temp == 0, 1, 0),
        .keep = c("none")
    ) %>% 
    pivot_longer(`ns(weighted_temp, df=5)`:temp, names_to="feature", values_to="value") %>% 
    mutate(
        feature = 
            factor(feature,
                   levels =
                       c(
                           "temp", "weighted_temp", "ns(weighted_temp, df=3)", "ns(weighted_temp, df=5)"
                       )
            )
    )

modspecs_precip <-
    grid %>% 
    left_join(
        fig2_mod_outputs %>% dplyr::select(model_number, model_rank),
        by=c("model_number")
    ) %>% 
    mutate(
        model_number = model_number,
        model_rank = model_rank,
        `ns(weighted_precip, df=5)` = ifelse(str_detect(controls, "ns\\(weighted_precip, df=5\\)"), 1, 0),
        `ns(weighted_precip, df=3)` = ifelse(str_detect(controls, "ns\\(weighted_precip, df=3\\)"), 1, 0),
        weighted_precip = ifelse(str_detect(controls, "weighted_precip") &
                                   `ns(weighted_precip, df=5)` == 0 &
                                   `ns(weighted_precip, df=3)` == 0, 1, 0),
        precip = ifelse(str_detect(controls, "precip") & 
                          `ns(weighted_precip, df=5)` == 0 &
                          `ns(weighted_precip, df=3)` == 0 & 
                          weighted_precip == 0, 1, 0),
        .keep = c("none")
    ) %>% 
    pivot_longer(`ns(weighted_precip, df=5)`:precip, names_to="feature", values_to="value") %>% 
    mutate(
        feature = 
            factor(feature,
                   levels =
                       c(
                           "precip", "weighted_precip", "ns(weighted_precip, df=3)", "ns(weighted_precip, df=5)"
                       )
            )
    )

#Create figure 2
coefs <-
    fig2_mod_outputs %>% 
    ggplot(aes(x = model_rank, y = point_est - 1)) + 
    geom_linerange(aes(ymin = CI_lower_clustered - 1, ymax = CI_upper_clustered - 1), lwd=.2) +
    geom_linerange(aes(ymin = CI_lower_iid - 1, ymax = CI_upper_iid - 1), lwd=.6, color = "cornflowerblue") +
    # scale_y_break(c(-.015, -.002)) +
    ylim(c(-.003, .005)) +
    geom_point(size=.5) +
    theme_minimal() +
    geom_hline(yintercept = 0, colour = DEFAULT_COLOR, lty = 2, size=0.25) + 
    # geom_hline(yintercept = -.015, colour = "red", lty = 2, size=0.25) + 
    # geom_hline(yintercept = -.002, colour = "red", lty = 2, size=0.25) + 
    theme(
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0,1,0,1), "cm")
    )


size <- .8
height <- .4
width <- .7

model_fes <-
    modspecs_fes %>% 
    ggplot(aes(x = model_rank, y = as.factor(feature), fill = color)) + 
    geom_tile(color = "gray", height = height, width = width) +
    scale_alpha_manual(NULL, values = c(0, 0, 1, .4)) +
    scale_y_discrete(labels = c("County", "County*Cal. Month", "Year-Month", "Year")) +
    theme_minimal() + 
    #scale_fill_gradient(low = "white", high = "black") +
    scale_fill_manual(values=c("white", "dodgerblue3", "coral2", "seagreen4")) +
    ylab("Fixed effects") +
    theme(
        legend.position = "none",
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8),
        panel.grid = element_blank(),
        plot.margin = unit(c(0,1,0,1), "cm")
    )

model_temp <-
    modspecs_temp %>% 
    ggplot(aes(x = model_rank, y = as.factor(feature), fill = value)) + 
    geom_tile(color = "gray", height = height, width = width) +
    scale_alpha_manual(NULL, values = c(0, 0, 1, .4)) +
    scale_y_discrete(labels = c("Linear, area-weighted", "Linear", "NS, df=3", "NS, df=5")) +
    theme_minimal() + 
    scale_fill_gradient(low = "white", high = "black") +
    ylab("Temperature") +
    theme(
        legend.position = "none",
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8),
        panel.grid = element_blank(),
        plot.margin = unit(c(0,1,0,1), "cm")
    )

model_precip <-
    modspecs_precip %>% 
    ggplot(aes(x = model_rank, y = as.factor(feature), fill = value)) + 
    geom_tile(color = "gray", height = height, width = width) +
    ggplot2::scale_alpha_manual(NULL, values = c(0, 0, 1, .4)) +
    scale_y_discrete(labels = c("Linear, area-weighted", "Linear", "NS, df=3", "NS, df=5")) +
    theme_minimal() + 
    scale_fill_gradient(low = "white", high = "black") +
    ylab("Precipitation") +
    ggplot2::theme(
        legend.position = "none",
        panel.border = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = element_text(size = 8),
        panel.grid = ggplot2::element_blank(),
        plot.margin = unit(c(0,1,0,1), "cm")
    )

fig2 <- coefs + model_fes + model_temp + model_precip + plot_layout(ncol = 1, heights = c(6,1,1,1))

ggsave(filename = "plots/fig2.png", plot = fig2, device = "png", dpi = 200, height = 5, width = 9)

#Create figure 3
fig3_data <-
    output %>% 
    dplyr::select(`Clustered at county level` = se_county_cluster, `IID` = se_iid, `Heteroskedasticity-robust` = se_hetero) %>% 
    pivot_longer(`Clustered at county level`:`Heteroskedasticity-robust`, names_to="se_type", values_to="value")

fig3 <-
    fig3_data %>% 
    ggplot(aes(x = value, group = se_type)) +
    geom_histogram(bins = 30) +
    xlim(c(0, .0015)) +
    facet_wrap(vars(se_type), ncol=1, scales = "free_y") +
    theme_minimal() +
    theme(
        #panel.grid = element_blank()
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
    )

ggsave(filename = "plots/fig3.png", plot = fig3, device = "png", dpi = 200, height = 5, width = 9)

# bin_num <- 30 #following bit is to allow separate x axes for each of the facets,but that may not be the point
# 
# fig3_iid <-
#     fig3_data %>% 
#     filter(se_type == "se_iid") %>% 
#     ggplot(aes(x = value)) +
#     geom_histogram(bins = bin_num) +
#     theme_minimal()
# 
# fig3_hetero <-
#     fig3_data %>% 
#     filter(se_type == "se_hetero") %>% 
#     ggplot(aes(x = value)) +
#     geom_histogram(bins = bin_num) +
#     theme_minimal()
# 
# fig3_cluster <-
#     fig3_data %>% 
#     filter(se_type == "se_county_cluster") %>% 
#     ggplot(aes(x = value)) +
#     geom_histogram(bins = bin_num) +
#     theme_minimal()
# 
# fig3_iid + fig3_hetero + fig3_cluster + plot_layout(ncol=1)


# fig3_barplotdata <-
#     output %>% 
#     left_join(
#         grid %>% dplyr::select(model_number, fes),
#         by=c("model_number")
#     ) %>% 
#     dplyr::select(fes, `Clustered at county level` = se_county_cluster, `IID` = se_iid, `Heteroskedasticity-robust` = se_hetero) %>% 
#     pivot_longer(`Clustered at county level`:`Heteroskedasticity-robust`, names_to="se_type", values_to="value")
# 
# fig3_barplotdata %>% 
#     ggplot(aes(x = value)) +
#     geom_histogram(bins = 20) +
#     #xlim(c(0, .0015)) +
#     facet_grid(fes ~ se_type, scales = "free") +
#     scale_x_continuous(guide = guide_axis(n.dodge=2)) +
#     theme_minimal() +
#     theme(
#         #panel.grid = element_blank()
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()
#     )
