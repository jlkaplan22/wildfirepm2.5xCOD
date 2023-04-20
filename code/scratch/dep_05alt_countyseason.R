#### Alternative FEs looking at county-season ####
# instead of county-month, then other FE is year
# Result: basically the same as county-month, which is fairly predictable

source("code/quickprepfinaldata_AC.R")

final <-
    final %>% 
    mutate(
        season =
            case_when(
                month == 1 | month == 2 | month == 3 ~ "winter",
                month == 4 | month == 5 | month == 6 ~ "spring",
                month == 7 | month == 8 | month == 9 ~ "summer",
                month == 10 | month == 11 | month == 12 ~ "fall"
            )
    )

#All-cause mortality and mean smoke PM2.5

# Run initial model for ACM with population-weighted temp and population-weighted precip
main_mod <- feglm(n_deaths ~ 
                      mean_pm2.5 + 
                      ns(weighted_temp, df=3) + weighted_precip | fipsihme^season + year, 
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
                               ns(temp, df=3) + precip | fipsihme^season + year, 
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
                           ns(weighted_temp, df=3) | fipsihme^season + year, 
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
                     ns(weighted_temp, df=5) + weighted_precip | fipsihme^season + year, 
                 offset = log(final$pop),
                 data = final,
                 weights = final$pop,
                 family = quasipoisson,
                 cluster = final$fipsihme)

summary(ns5_mod)
interpret_model(ns5_mod, "cluster")
interpret_model(ns5_mod, "hetero")

# Forest plot (base code from Jeff's SEDA paper)
mod_list <-
    rbind(
        main_mod$coeftable %>% data.frame() %>% mutate(mod_name = "main", var=rownames(.)),
        area_weighted_mod$coeftable %>% data.frame() %>% mutate(mod_name = "area-weighted env vars", var=rownames(.)),
        no_precip_mod$coeftable %>% data.frame() %>% mutate(mod_name = "no precip var", var=rownames(.)),
        ns5_mod$coeftable %>% data.frame() %>% mutate(mod_name = "temp ns, df=5", var=rownames(.))
    )

mod_df <- 
    mod_list %>%
    filter(grepl("mean_pm2.5", var)) %>% 
    rename(est=Estimate, se=`Std..Error`, t_val=`t.value`, p_val=`Pr...t..`) %>% 
    mutate(
        point_est = exp(est),
        CI_lower = exp(est - qnorm(0.975) * se),
        CI_upper = exp(est + qnorm(0.975) * se),
        
        mod_name = factor(mod_name, levels = c("main", "area-weighted env vars", "no precip var", "temp ns, df=5"))
    )

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
        subtitle="FEs for county-season and year",
        y=expression(atop("%"~Delta~" Mortality Rate per", "1"~mu*"g m"^"-3")), 
        x="", 
        color="Subject"
    ) + 
    scale_y_continuous(labels=scales::percent_format(accuracy=.01, suffix="")) +
    scale_x_discrete(guide = guide_axis(n.dodge=2))
