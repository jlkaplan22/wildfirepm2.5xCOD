#### TWFEs for marital status sub-analysis ####
# working on it during week of 3/13/23

source("code/quickprepfinaldata_MS.R")

#All-cause mortality and mean smoke PM2.5 stratified by marital status


# Run initial model for ACM with population-weighted temp and population-weighted precip
# (just to ensure same results as in `dev_05_initial_models.R`)
main_mod <- subanalysis_modeler(final %>% filter(sex == "all", marital == "all"))

summary(main_mod)
interpret_model(main_mod, "cluster")

# Never married
never_married_mod <- subanalysis_modeler(final %>% filter(sex == "all", marital == "single_never_married"))
summary(never_married_mod)

# Now married
married_mod <- subanalysis_modeler(final %>% filter(sex == "all", marital == "married"))
summary(married_mod)

# Divorced
divorced_mod <- subanalysis_modeler(final %>% filter(sex == "all", marital == "divorced"))
summary(divorced_mod)

# Widowed
widowed_mod <- subanalysis_modeler(final %>% filter(sex == "all", marital == "widowed"))
summary(widowed_mod)

#Check coefficients
interpret_model(never_married_mod, "cluster")
interpret_model(married_mod, "cluster")
interpret_model(divorced_mod, "cluster")
interpret_model(widowed_mod, "cluster")


# Forest plot (base code from Jeff's SEDA paper)
mod_list <-
    rbind(
        main_mod$coeftable %>% data.frame() %>% mutate(mod_name = "all", var=rownames(.)),
        never_married_mod$coeftable %>% data.frame() %>% mutate(mod_name = "never_married", var=rownames(.)),
        married_mod$coeftable %>% data.frame() %>% mutate(mod_name = "married", var=rownames(.)),
        divorced_mod$coeftable %>% data.frame() %>% mutate(mod_name = "divorced", var=rownames(.)),
        widowed_mod$coeftable %>% data.frame() %>% mutate(mod_name = "widowed", var=rownames(.))
    )

mod_df <- 
    mod_list %>%
    filter(grepl("mean_pm2.5", var)) %>% 
    rename(est=Estimate, se=`Std..Error`, t_val=`t.value`, p_val=`Pr...t..`) %>% 
    mutate(
        point_est = exp(est),
        CI_lower = exp(est - qnorm(0.975) * se),
        CI_upper = exp(est + qnorm(0.975) * se),
        
        mod_name = factor(mod_name, levels = c("all", "never_married", "married", "divorced", "widowed"))
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
        title="Wildfire smoke PM2.5 x AC mortality, by marital status",
        subtitle="FEs for county-month and year",
        y=expression(atop(Delta~" Mortality Rate per", "1"~mu*"g m"^"-3")), x="", color="Subject"
    ) + 
    scale_y_continuous(labels=scales::percent_format(accuracy=.01, suffix="")) +
    scale_x_discrete(guide = guide_axis(n.dodge=2))

##### Repeat analysis, stratifying by sex as well ####
main_mod_male <- subanalysis_modeler(final %>% filter(sex == "male", marital == "all"))
main_mod_female <- subanalysis_modeler(final %>% filter(sex == "female", marital == "all"))

never_married_mod_male <- subanalysis_modeler(final %>% filter(sex == "male", marital == "single_never_married"))
never_married_mod_female <- subanalysis_modeler(final %>% filter(sex == "female", marital == "single_never_married"))

married_mod_male <- subanalysis_modeler(final %>% filter(sex == "male", marital == "married"))
married_mod_female <- subanalysis_modeler(final %>% filter(sex == "female", marital == "married"))

divorced_mod_male <- subanalysis_modeler(final %>% filter(sex == "male", marital == "divorced"))
divorced_mod_female <- subanalysis_modeler(final %>% filter(sex == "female", marital == "divorced"))

widowed_mod_male <- subanalysis_modeler(final %>% filter(sex == "male", marital == "widowed"))
widowed_mod_female <- subanalysis_modeler(final %>% filter(sex == "female", marital == "widowed"))

#Forest plot
# Forest plot (base code from Jeff's SEDA paper)
mod_list <-
    rbind(
        main_mod_male$coeftable %>% data.frame() %>% mutate(mod_name = "all males", var=rownames(.)),
        main_mod_female$coeftable %>% data.frame() %>% mutate(mod_name = "all females", var=rownames(.)),
        
        never_married_mod_male$coeftable %>% data.frame() %>% mutate(mod_name = "never married, males", var=rownames(.)),
        never_married_mod_female$coeftable %>% data.frame() %>% mutate(mod_name = "never married, females", var=rownames(.)),
        
        married_mod_male$coeftable %>% data.frame() %>% mutate(mod_name = "married, males", var=rownames(.)),
        married_mod_female$coeftable %>% data.frame() %>% mutate(mod_name = "married, females", var=rownames(.)),
        
        divorced_mod_male$coeftable %>% data.frame() %>% mutate(mod_name = "divorced, males", var=rownames(.)),
        divorced_mod_female$coeftable %>% data.frame() %>% mutate(mod_name = "divorced, females", var=rownames(.)),
        
        widowed_mod_male$coeftable %>% data.frame() %>% mutate(mod_name = "widowed, males", var=rownames(.)),
        widowed_mod_female$coeftable %>% data.frame() %>% mutate(mod_name = "widowed, females", var=rownames(.))
        
    )

mod_df <- 
    mod_list %>%
    filter(grepl("mean_pm2.5", var)) %>% 
    rename(est=Estimate, se=`Std..Error`, t_val=`t.value`, p_val=`Pr...t..`) %>% 
    mutate(
        point_est = exp(est),
        CI_lower = exp(est - qnorm(0.975) * se),
        CI_upper = exp(est + qnorm(0.975) * se),
        
        mod_name = factor(mod_name, levels = 
                              c("all males", "all females", 
                                "never married, males", "never married, females",
                                "married, males", "married, females",
                                "divorced, males",  "divorced, females",
                                "widowed, males", "widowed, females"))
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
        title="Wildfire smoke PM2.5 x AC mortality, by marital status and sex",
        subtitle="FEs for county-month and year",
        y=expression(atop(Delta~" Mortality Rate per", "1"~mu*"g m"^"-3")), x="", color="Subject"
    ) + 
    scale_y_continuous(labels=scales::percent_format(accuracy=.01, suffix="")) +
    scale_x_discrete(guide = guide_axis(n.dodge=2))


