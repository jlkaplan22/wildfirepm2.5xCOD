#### Checking to see that gnm and fixest packages converge ####
source("code/quickprepfinaldata_AC.R")


#Primary models with county-month and year FEs:
main_mod <- feglm(n_deaths ~ 
                      mean_pm2.5 + 
                      ns(weighted_temp, df=3) + weighted_precip | fipsihme^month + year, 
                  offset = log(final$pop),
                  data = final,
                  weights = final$pop,
                  family = quasipoisson,
                  cluster = final$fipsihme)

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

summary(main_mod, vcov = "iid")
summary(mod_gnm_countymonth)$coefficients[1,]

#Primary models with year-month and county FEs:
main_mod_yearmonth <- feglm(n_deaths ~ 
                      mean_pm2.5 + 
                      ns(weighted_temp, df=3) + weighted_precip | fipsihme + yearmonth, 
                  offset = log(final$pop),
                  data = final,
                  weights = final$pop,
                  family = quasipoisson,
                  cluster = final$fipsihme)


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

summary(main_mod_yearmonth, vcov = "iid")
summary_mod_gnm_yearmonth <- summary(mod_gnm_yearmonth) #takes a long time to run
summary_mod_gnm_yearmonth$coefficients[1,]
# estimate for mean_pm2.5: 2.380e-04, se = 1.200e-04, p = 0.0474

