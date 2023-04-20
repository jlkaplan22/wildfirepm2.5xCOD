#### Quick test of main models when using the full range of years ####
#basically if we decide to drop marital status as EMM, then no reason to
#restrict to 2009-2020

source("code/quickprepfinaldata_AC.R") #need to edit this code quickly to change the years

# Run initial model for ACM with population-weighted temp and population-weighted precip
main_mod <- feglm(n_deaths ~ 
                      mean_pm2.5 + 
                      ns(weighted_temp, df=3) + weighted_precip | fipsihme^month + year, 
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
                               ns(temp, df=3) + precip | month^fipsihme + year, 
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
                           ns(weighted_temp, df=3) | month^fipsihme + year, 
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
                     ns(weighted_temp, df=5) + weighted_precip | month^fipsihme + year, 
                 offset = log(final$pop),
                 data = final,
                 weights = final$pop,
                 family = quasipoisson,
                 cluster = final$fipsihme)

summary(ns5_mod)
interpret_model(ns5_mod, "cluster")
interpret_model(ns5_mod, "hetero")

# Model run with `gnm` package
library(gnm)

final_gnm <-
    final %>% 
    unite(county_month, c(fipsihme, month), sep="_")

mod_gnm_countymonth <-
    gnm(n_deaths ~
            mean_pm2.5 +
            ns(weighted_temp, df=3) + 
            weighted_precip + 
            as.factor(year),
        family = quasipoisson,
        eliminate = as.factor(county_month),
        offset = log(final_gnm$pop),
        weights = final_gnm$pop,
        data=final_gnm
    )
summary(mod_gnm_countymonth)
interpret_model(mod_gnm_countymonth, "")
interpret_model(main_mod, "cluster") #compare to fixest version


# Forest plot (base code from Jeff's SEDA paper)
mod_list <-
    rbind(
        main_mod$coeftable %>% data.frame() %>% mutate(mod_name = "main", var=rownames(.)),
        area_weighted_mod$coeftable %>% data.frame() %>% mutate(mod_name = "area-weighted env vars", var=rownames(.)),
        no_precip_mod$coeftable %>% data.frame() %>% mutate(mod_name = "no precip var", var=rownames(.)),
        ns5_mod$coeftable %>% data.frame() %>% mutate(mod_name = "temp ns, df=5", var=rownames(.))
    ) %>% 
    dplyr::select(-`t.value`, -`Pr...t..`) %>% 
    rbind(
        se(mod_gnm_countymonth)[1,] %>% data.frame() %>% mutate(mod_name = "gnm", var=rownames(.))
    )

mod_df <- 
    mod_list %>%
    filter(grepl("mean_pm2.5", var)) %>% 
    rename(est=Estimate, se=`Std..Error`) %>% 
    mutate(
        point_est = exp(est),
        CI_lower = exp(est - qnorm(0.975) * se),
        CI_upper = exp(est + qnorm(0.975) * se),
        
        mod_name = factor(mod_name, levels = c("main", "area-weighted env vars", "no precip var", "temp ns, df=5", "gnm"))
    )

countymonth_main_regs <-
    ggplot(data = mod_df) + #exponentiate to get the risk increase for 1 µg/m3 wfpm2.5
    base_theme + 
    theme(
        legend.position = "none"
    ) + 
    geom_hline(yintercept = 0, colour = DEFAULT_COLOR, lty = 2, size=0.25) + 
    geom_linerange(aes(x=mod_name, ymin=CI_lower-1, ymax=CI_upper-1), lwd=0.75,position=position_dodge2(width=.25), color=LINE_COLOR) + 
    geom_point(aes(x=mod_name, y=point_est-1), position=position_dodge2(width=.25), shape = 21, fill = "WHITE", color=LINE_COLOR, size=3) + 
    labs(
        title="Wildfire smoke PM2.5 x all-cause mortality, 2006-2020",
        subtitle="FEs for county-month and year",
        y=expression(atop("%"~Delta~" Mortality Rate per", "1"~mu*"g m"^"-3")), 
        x="", 
        color="Subject"
    ) + 
    scale_y_continuous(labels=scales::percent_format(accuracy=.01, suffix="")) +
    scale_x_discrete(guide = guide_axis(n.dodge=2))


