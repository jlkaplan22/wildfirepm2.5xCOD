#### TWFEs for main analysis ####
# working on it during week of 3/6/23

source("code/quickprepfinaldata_AC.R")

#All-cause mortality and mean smoke PM2.5

# Run initial model for ACM with population-weighted temp and population-weighted precip
main_mod <- feglm(n_deaths ~ 
                      mean_pm2.5 + 
                      ns(weighted_temp, df=3) + weighted_precip | fipsihme + yearmonth, 
                  offset = log(final$pop),
                  data = final,
                  weights = final$pop,
                  family = quasipoisson,
                  cluster = final$fipsihme)

summary(main_mod)
interpret_model(main_mod, "cluster")
interpret_model(main_mod, "hetero")

# Run sensitivity analysis using area-weighted values for temp and precip
area_weighted_mod <- feglm(n_deaths ~ 
                               mean_pm2.5 + 
                               ns(temp, df=3) + precip | fipsihme + yearmonth, 
                           offset = log(final$pop),
                           data = final,
                           weights = final$pop,
                           family = quasipoisson,
                           cluster = final$fipsihme)

summary(area_weighted_mod)
interpret_model(area_weighted_mod, "cluster")
interpret_model(area_weighted_mod, "hetero")

# Run sensitivity analysis without precipitation
no_precip_mod <- feglm(n_deaths ~ 
                           mean_pm2.5 + 
                           ns(weighted_temp, df=3) | fipsihme + yearmonth, 
                       offset = log(final$pop),
                       data = final,
                       weights = final$pop,
                       family = quasipoisson,
                       cluster = final$fipsihme)

summary(no_precip_mod)
interpret_model(no_precip_mod, "cluster")
interpret_model(no_precip_mod, "hetero")

# Run sensitivity analysis using 5 dfs for temp ns instead of 3
ns5_mod <- feglm(n_deaths ~ 
                     mean_pm2.5 + 
                     ns(weighted_temp, df=5) + weighted_precip | fipsihme + yearmonth, 
                 offset = log(final$pop),
                 data = final,
                 weights = final$pop,
                 family = quasipoisson,
                 cluster = final$fipsihme)

summary(ns5_mod)
interpret_model(ns5_mod, "cluster")
interpret_model(ns5_mod, "hetero")

# gnm model with yearmonth FE
library(gnm)
mod_gnm_yearmonth <-
    gnm(n_deaths ~
            mean_pm2.5 +
            ns(weighted_temp, df=3) + 
            weighted_precip + 
            as.factor(yearmonth),
        family = quasipoisson,
        offset = log(final_gnm$pop),
        eliminate = as.factor(fipsihme),
        weights = final$pop,
        data=final
    )

tic()
summary_mod_gnm_yearmonth <- summary(mod_gnm_yearmonth) #takes a long time to run
# estimate for mean_pm2.5: 2.380e-04, se = 1.200e-04, p = 0.0474
toc() #1649.002 sec elapsed

summary_mod_gnm_yearmonth$coefficients[1,] #save the coef and se for pm2.5

tic()
interpret_model(mod_gnm_yearmonth, "") #also takes a long time to run
#"% change = 0.0238 (0.0002715, 0.04733)"
toc() #2506.674 sec elapsed


# Forest plot (base code from Jeff's SEDA paper)
mod_list <-
    rbind(
        main_mod$coeftable %>% data.frame() %>% mutate(mod_name = "main", var=rownames(.)),
        area_weighted_mod$coeftable %>% data.frame() %>% mutate(mod_name = "area-weighted env vars", var=rownames(.)),
        no_precip_mod$coeftable %>% data.frame() %>% mutate(mod_name = "no precip var", var=rownames(.)),
        ns5_mod$coeftable %>% data.frame() %>% mutate(mod_name = "temp ns, df=5", var=rownames(.)),
        summary_mod_gnm_yearmonth$coefficients[1,] %>% data.frame() %>% t() %>% data.frame() %>% mutate(mod_name = "gnm", var="mean_pm2.5")
    ) 

mod_df <- 
    mod_list %>%
    filter(grepl("mean_pm2.5", var)) %>% 
    rename(est=Estimate, se=`Std..Error`, t_val=`t.value`, p_val=`Pr...t..`) %>% 
    mutate(
        point_est = exp(est),
        CI_lower = exp(est - qnorm(0.975) * se),
        CI_upper = exp(est + qnorm(0.975) * se),
        
        mod_name = factor(mod_name, levels = c("main", "area-weighted env vars", "no precip var", "temp ns, df=5", "gnm"))
    )

yearmonth_main_regs <-
    ggplot(data = mod_df) + #exponentiate to get the risk increase for 1 µg/m3 wfpm2.5
    base_theme + 
    theme(
        legend.position = "none"
    ) + 
    geom_hline(yintercept = 0, colour = DEFAULT_COLOR, lty = 2, size=0.25) + 
    geom_linerange(aes(x=mod_name, ymin=CI_lower-1, ymax=CI_upper-1), lwd=0.75,position=position_dodge2(width=.25), color=LINE_COLOR) + 
    geom_point(aes(x=mod_name, y=point_est-1), position=position_dodge2(width=.25), shape = 21, fill = "WHITE", color=LINE_COLOR, size=3) + 
    labs(
        title="Wildfire smoke PM2.5 x all-cause mortality",
        subtitle="FEs for county and month-year",
        y=expression(atop("%"~Delta~" Mortality Rate per", "1"~mu*"g m"^"-3")), 
        x="", 
        color="Subject"
    ) + 
    scale_y_continuous(labels=scales::percent_format(accuracy=.01, suffix="")) +
    scale_x_discrete(guide = guide_axis(n.dodge=2))

ggsave(filename = "plots/yearmonth_main_regs.png", plot = yearmonth_main_regs, device = "png", dpi = 200)
