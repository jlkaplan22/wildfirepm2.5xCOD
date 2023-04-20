### Tidy data and run initial models ####
# Trying to replicate main findings from Ma et al. 2023 (preprint as of now)
rm(list = ls())
source("code/00_config.R")

#Tidy final data for ACM
PRISM <- read_csv(paste0(data_dir, "prism_monthly.csv"))

# aggregate to monthly level and create thresholding features
avg_county_smokePM <- readRDS("data/county_smokePM_predictions_20060101_20201231.rds")
county_smokePM_features <-
    avg_county_smokePM %>% 
    mutate(year = year(date), month = month(date)) %>% 
    group_by(GEOID, year, month) %>% 
    summarise(
        cum_pm2.5 = sum(smokePM_pred),
        mean_pm2.5 =
            case_when(
                month %in% c(1, 3, 5, 7, 8, 10, 12) ~ cum_pm2.5 / 31,
                month %in% c(4, 6, 9, 11) ~ cum_pm2.5 / 30,
                month %in% c(2) ~ cum_pm2.5 / 28
            ),
        daysover0 = sum(smokePM_pred > 0),
        daysover5 = sum(smokePM_pred > 5),
        daysover12.5 = sum(smokePM_pred > 12.5),
        daysover20 = sum(smokePM_pred > 20),
        daysover40 = sum(smokePM_pred > 40)
    ) %>% 
    distinct()

#Load all-cause mortality data
COD <- 
    readRDS(paste0(raw_data_dir, "age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-all_cause.RDS")) %>% 
    mutate(statefips = str_sub(fipsihme, 1, 2)) %>% 
    filter(statefips %in% non_CONUS_FIPS == FALSE) %>%  
    filter(
        year >= 2006,
        year <= 2016,
        race_eth == "all",
        age_group == "all_ages",
        marital == "all", 
        sex == "all",
        death_type == "all_cause"
    )


final <-
    COD %>% 
    left_join(
        PRISM, 
        by=c("fipsihme" = "GEOID", "year", "month")
    ) %>% 
    left_join(
        county_smokePM_features,
        by=c("fipsihme" = "GEOID", "year", "month")
    ) %>% 
    mutate(
        mean_pm2.5 = ifelse(is.na(mean_pm2.5), 0, mean_pm2.5),
        cum_pm2.5 = ifelse(is.na(cum_pm2.5), 0, cum_pm2.5),
        daysover0 = ifelse(is.na(daysover0), 0, daysover0),
        daysover5 = ifelse(is.na(daysover5), 0, daysover5),
        daysover12.5 = ifelse(is.na(daysover12.5), 0, daysover12.5),
        daysover20 = ifelse(is.na(daysover20), 0, daysover20),
        daysover40 = ifelse(is.na(daysover40), 0, daysover40),
        
        yearmonth = ym(paste(year, month, sep = "-"))
    )

#Tidy final data for cause-specific mortality
final_cvd <-
    readRDS(paste0(raw_data_dir, "age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-cardiovascular.RDS")) %>% 
    mutate(statefips = str_sub(fipsihme, 1, 2)) %>% 
    filter(statefips %in% non_CONUS_FIPS == FALSE) %>%  
    filter(
        year >= 2006, #replicating Ma et al as of 2/23/23
        year <= 2016,
        race_eth == "all",
        age_group == "all_ages",
        sex == "all",
        marital == "all",
        death_type == "cardiovascular"
    ) %>% 
    left_join(
        PRISM, 
        by=c("fipsihme" = "GEOID", "year", "month")
    ) %>% 
    left_join(
        county_smokePM_features,
        by=c("fipsihme" = "GEOID", "year", "month")
    ) %>% 
    mutate(
        mean_pm2.5 = ifelse(is.na(mean_pm2.5), 0, mean_pm2.5),
        cum_pm2.5 = ifelse(is.na(cum_pm2.5), 0, cum_pm2.5),
        daysover0 = ifelse(is.na(daysover0), 0, daysover0),
        daysover5 = ifelse(is.na(daysover5), 0, daysover5),
        daysover12.5 = ifelse(is.na(daysover12.5), 0, daysover12.5),
        daysover20 = ifelse(is.na(daysover20), 0, daysover20),
        daysover40 = ifelse(is.na(daysover40), 0, daysover40),
        
        yearmonth = ym(paste(year, month, sep = "-"))
    )

# final_resp <-
#     resp %>% 
#     left_join(
#         PRISM, 
#         by=c("fipsihme" = "GEOID", "year", "month")
#     ) %>% 
#     left_join(
#         county_smokePM_features,
#         by=c("fipsihme" = "GEOID", "year", "month")
#     ) %>% 
#     mutate(
#         mean_pm2.5 = ifelse(is.na(mean_pm2.5), 0, mean_pm2.5),
#         cum_pm2.5 = ifelse(is.na(cum_pm2.5), 0, cum_pm2.5),
#         daysover0 = ifelse(is.na(daysover0), 0, daysover0),
#         daysover5 = ifelse(is.na(daysover5), 0, daysover5),
#         daysover12.5 = ifelse(is.na(daysover12.5), 0, daysover12.5),
#         daysover20 = ifelse(is.na(daysover20), 0, daysover20),
#         daysover40 = ifelse(is.na(daysover40), 0, daysover40),
#         
#         yearmonth = ym(paste(year, month, sep = "-"))
#     )
# 
# final_neuropsych <-
#     neuropsych %>% 
#     left_join(
#         PRISM_popw, 
#         by=c("fipsihme" = "GEOID", "year", "month")
#     ) %>% 
#     left_join(
#         county_smokePM_features,
#         by=c("fipsihme" = "GEOID", "year", "month")
#     ) %>% 
#     mutate(
#         mean_pm2.5 = ifelse(is.na(mean_pm2.5), 0, mean_pm2.5),
#         cum_pm2.5 = ifelse(is.na(cum_pm2.5), 0, cum_pm2.5),
#         daysover0 = ifelse(is.na(daysover0), 0, daysover0),
#         daysover5 = ifelse(is.na(daysover5), 0, daysover5),
#         daysover12.5 = ifelse(is.na(daysover12.5), 0, daysover12.5),
#         daysover20 = ifelse(is.na(daysover20), 0, daysover20),
#         daysover40 = ifelse(is.na(daysover40), 0, daysover40),
#         
#         yearmonth = ym(paste(year, month, sep = "-"))
#     )
# 
# final_injuries <-
#     injuries %>% 
#     left_join(
#         PRISM_popw, 
#         by=c("fipsihme" = "GEOID", "year", "month")
#     ) %>% 
#     left_join(
#         county_smokePM_features,
#         by=c("fipsihme" = "GEOID", "year", "month")
#     ) %>% 
#     mutate(
#         mean_pm2.5 = ifelse(is.na(mean_pm2.5), 0, mean_pm2.5),
#         cum_pm2.5 = ifelse(is.na(cum_pm2.5), 0, cum_pm2.5),
#         daysover0 = ifelse(is.na(daysover0), 0, daysover0),
#         daysover5 = ifelse(is.na(daysover5), 0, daysover5),
#         daysover12.5 = ifelse(is.na(daysover12.5), 0, daysover12.5),
#         daysover20 = ifelse(is.na(daysover20), 0, daysover20),
#         daysover40 = ifelse(is.na(daysover40), 0, daysover40),
#         
#         yearmonth = ym(paste(year, month, sep = "-"))
#     )
# 
# final_infectious_parasitic <-
#     infectious_parasitic %>% 
#     left_join(
#         PRISM_popw, 
#         by=c("fipsihme" = "GEOID", "year", "month")
#     ) %>% 
#     left_join(
#         county_smokePM_features,
#         by=c("fipsihme" = "GEOID", "year", "month")
#     ) %>% 
#     mutate(
#         mean_pm2.5 = ifelse(is.na(mean_pm2.5), 0, mean_pm2.5),
#         cum_pm2.5 = ifelse(is.na(cum_pm2.5), 0, cum_pm2.5),
#         daysover0 = ifelse(is.na(daysover0), 0, daysover0),
#         daysover5 = ifelse(is.na(daysover5), 0, daysover5),
#         daysover12.5 = ifelse(is.na(daysover12.5), 0, daysover12.5),
#         daysover20 = ifelse(is.na(daysover20), 0, daysover20),
#         daysover40 = ifelse(is.na(daysover40), 0, daysover40),
#         
#         yearmonth = ym(paste(year, month, sep = "-"))
#     )
# 
# final_cancer <-
#     cancer %>% 
#     left_join(
#         PRISM_popw, 
#         by=c("fipsihme" = "GEOID", "year", "month")
#     ) %>% 
#     left_join(
#         county_smokePM_features,
#         by=c("fipsihme" = "GEOID", "year", "month")
#     ) %>% 
#     mutate(
#         mean_pm2.5 = ifelse(is.na(mean_pm2.5), 0, mean_pm2.5),
#         cum_pm2.5 = ifelse(is.na(cum_pm2.5), 0, cum_pm2.5),
#         daysover0 = ifelse(is.na(daysover0), 0, daysover0),
#         daysover5 = ifelse(is.na(daysover5), 0, daysover5),
#         daysover12.5 = ifelse(is.na(daysover12.5), 0, daysover12.5),
#         daysover20 = ifelse(is.na(daysover20), 0, daysover20),
#         daysover40 = ifelse(is.na(daysover40), 0, daysover40),
#         
#         yearmonth = ym(paste(year, month, sep = "-"))
#     )

# Run initial model for ACM with area-weighted temp
mod1 <- feglm(n_deaths ~ 
                  mean_pm2.5 + 
                  ns(temp, df=5) | yearmonth + fipsihme, 
              offset = log(final$pop),
              data = final,
              weights = final$pop,
              family = quasipoisson,
              cluster = final$fipsihme)

summary(mod1)
summary(mod1, vcov="iid") #this replicates Ma et al. results precisely

interpret_model(mod1, "cluster")
interpret_model(mod1, "hetero")
interpret_model(mod1, "iid")

mod1_FEs <- fixef(mod1)

# same model but with county^month FEs
mod1_countymonthFE <- feglm(n_deaths ~ 
                  mean_pm2.5 + 
                  ns(temp, df=5) | fipsihme^month + year, 
              offset = log(final$pop),
              data = final,
              weights = final$pop,
              family = quasipoisson,
              cluster = final$fipsihme)

summary(mod1_countymonthFE)
interpret_model(mod1_countymonthFE, "cluster")

# Run initial model for ACM with population-weighted temp
# mod1_pop <- feglm(n_deaths ~ 
#                   mean_pm2.5 + 
#                   ns(weighted_temp, df=5) | yearmonth + fipsihme, 
#               offset = log(final_popw$pop),
#               data = final_popw,
#               weights = final_popw$pop,
#               family = quasipoisson,
#               cluster = final_popw$fipsihme)
# 
# summary(mod1_pop)
# interpret_model(mod1_pop, "hetero")
# interpret_model(mod1_pop, "cluster")

# Add pop-weighted precip
# mod1_pop_precip <- feglm(n_deaths ~ 
#                       mean_pm2.5 + 
#                       ns(weighted_temp, df=5) +
#                       weighted_precip | yearmonth + fipsihme, 
#                   offset = log(final_popw$pop),
#                   data = final_popw,
#                   weights = final_popw$pop,
#                   family = quasipoisson,
#                   cluster = final_popw$fipsihme)
# 
# summary(mod1_pop_precip)
# interpret_model(mod1_pop_precip, "hetero")
# interpret_model(mod1_pop_precip, "cluster")

# Try initial model using `pglm` package
# mod1_pglm <- pglm(n_deaths ~ 
#                       mean_pm2.5 + 
#                       ns(weighted_temp, df=5) +
#                       as.factor(yearmonth) +
#                       as.factor(fipsihme),
#                   data = final,
#                   model = "pooling",
#                   family = quasipoisson
#                   )

# Initial model for CVD mortality (area-weighted temp)
mod2 <- feglm(n_deaths ~ 
                  mean_pm2.5 + 
                  ns(temp, df=5) | yearmonth + fipsihme, 
              offset = log(final_cvd$pop),
              data = final_cvd,
              weights = final_cvd$pop,
              family = quasipoisson,
              cluster = final_cvd$fipsihme)

summary(mod2)
summary(mod2, vcov="iid")
interpret_model(mod2, "hetero")
interpret_model(mod2, "iid")

# CVD mortality with county-month FE
mod2_countymonthFE <- feglm(n_deaths ~ 
                  mean_pm2.5 + 
                  ns(temp, df=5) | fipsihme^month + year, 
              offset = log(final_cvd$pop),
              data = final_cvd,
              weights = final_cvd$pop,
              family = quasipoisson,
              cluster = final_cvd$fipsihme)

# Intial model for CVD mortality (population-weighted temp)
# mod2_popw <- feglm(n_deaths ~ 
#                   mean_pm2.5 + 
#                   ns(weighted_temp, df=5) | yearmonth + fipsihme, 
#               offset = log(final_cvd_popw$pop),
#               data = final_cvd_popw,
#               weights = final_cvd_popw$pop,
#               family = quasipoisson,
#               cluster = final_cvd_popw$fipsihme)
# 
# summary(mod2_popw)
# interpret_model(mod2_popw)

# Initial model for resp mortality (area-weighted temp)
# mod3 <- feglm(n_deaths ~ 
#                   mean_pm2.5 + 
#                   ns(temp, df=5) | yearmonth + fipsihme, 
#               offset = log(final_resp$pop),
#               data = final_resp,
#               weights = final_resp$pop,
#               family = quasipoisson,
#               cluster = final_resp$fipsihme)
# 
# summary(mod3)
# summary(mod3, vcov="iid")
# interpret_model(mod3, "hetero")
# interpret_model(mod3, "iid")

# Initial model for resp mortality (population-weighted temp)
# mod3_popw <- feglm(n_deaths ~ 
#                   mean_pm2.5 + 
#                   ns(weighted_temp, df=5) | yearmonth + fipsihme, 
#               offset = log(final_resp_popw$pop),
#               data = final_resp_popw,
#               weights = final_resp_popw$pop,
#               family = quasipoisson,
#               cluster = final_resp_popw$fipsihme)
# 
# summary(mod3_popw)
# interpret_model(mod3_popw)

# Initial model for neuropsychiatric (population-weighted temp)
# mod_neuropsych <- feglm(n_deaths ~ 
#                        mean_pm2.5 + 
#                        ns(weighted_temp, df=5) | yearmonth + fipsihme, 
#                    offset = log(final_neuropsych$pop),
#                    data = final_neuropsych,
#                    weights = final_neuropsych$pop,
#                    family = quasipoisson,
#                    cluster = final_neuropsych$fipsihme)
# 
# summary(mod_neuropsych)
# interpret_model(mod_neuropsych, "cluster")

# Initial model for injuries (population-weighted temp)
# mod_injuries <- feglm(n_deaths ~ 
#                             mean_pm2.5 + 
#                             ns(weighted_temp, df=5) | yearmonth + fipsihme, 
#                         offset = log(final_injuries$pop),
#                         data = final_injuries,
#                         weights = final_injuries$pop,
#                         family = quasipoisson,
#                         cluster = final_injuries$fipsihme)
# 
# summary(mod_injuries)
# interpret_model(mod_injuries, "cluster")

# Additional model for injuries (population-weighted temp, and pop-weighted precip)
# mod_injuries_precip <- feglm(n_deaths ~ 
#                           mean_pm2.5 + 
#                           ns(weighted_temp, df=5) +
#                           weighted_precip | yearmonth + fipsihme, 
#                       offset = log(final_injuries$pop),
#                       data = final_injuries,
#                       weights = final_injuries$pop,
#                       family = quasipoisson,
#                       cluster = final_injuries$fipsihme)
# 
# summary(mod_injuries_precip)
# interpret_model(mod_injuries_precip, "cluster")

# Initial model for infectious+parasitic disease (population-weighted temp)
# mod_infectious_parasitic <- feglm(n_deaths ~ 
#                           mean_pm2.5 + 
#                           ns(weighted_temp, df=5) | yearmonth + fipsihme, 
#                       offset = log(final_infectious_parasitic$pop),
#                       data = final_infectious_parasitic,
#                       weights = final_infectious_parasitic$pop,
#                       family = quasipoisson,
#                       cluster = final_infectious_parasitic$fipsihme)
# 
# summary(mod_infectious_parasitic)
# interpret_model(mod_infectious_parasitic, "cluster")

# Initial model for infectious+parasitic disease (population-weighted temp)
# mod_infectious_cancer <- feglm(n_deaths ~ 
#                                       mean_pm2.5 + 
#                                       ns(weighted_temp, df=5) | yearmonth + fipsihme, 
#                                   offset = log(final_cancer$pop),
#                                   data = final_cancer,
#                                   weights = final_cancer$pop,
#                                   family = quasipoisson,
#                                   cluster = final_cancer$fipsihme)
# 
# summary(mod_infectious_cancer)
# interpret_model(mod_infectious_cancer, "cluster")

# Forest plot (base code from Jeff's SEDA paper)
mod_list <-
    rbind(
        mod1$coeftable %>% data.frame() %>% mutate(mod_name = "all-cause", var=rownames(.)),
        mod2$coeftable %>% data.frame() %>% mutate(mod_name = "cardiovascular", var=rownames(.)),
        mod1_countymonthFE$coeftable %>% data.frame() %>% mutate(mod_name = "all-cause, county-month FE", var=rownames(.)),
        mod2_countymonthFE$coeftable %>% data.frame() %>% mutate(mod_name = "cv, county-month FE", var=rownames(.))
    )

mod_df <- 
    mod_list %>%
    filter(grepl("mean_pm2.5", var)) %>% 
    rename(est=Estimate, se=`Std..Error`, t_val=`t.value`, p_val=`Pr...t..`) %>% 
    mutate(
        point_est = exp(est),
        CI_lower = exp(est - qnorm(0.975) * se),
        CI_upper = exp(est + qnorm(0.975) * se),
        
        mod_name = factor(mod_name, levels = c("all-cause", "cardiovascular", "all-cause, county-month FE", "cv, county-month FE"))
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
        title="Wildfire smoke PM2.5 x all-cause mortality, ",
        subtitle="Ma et al. replication (but clustered SEs instead of IID SEs)",
        y=expression(atop("%"~Delta~" Mortality Rate per", "1"~mu*"g m"^"-3")), 
        x="", 
        color="Subject"
    ) + 
    scale_y_continuous(labels=scales::percent_format(accuracy=.01, suffix="")) +
    scale_x_discrete(guide = guide_axis(n.dodge=2))

