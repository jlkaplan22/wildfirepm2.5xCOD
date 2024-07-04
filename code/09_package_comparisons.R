### Compare `fixest`, `gnm`, and others ###

#2 lines below are for development purposes, speeding things up #!#!#!
source("code/00_config.R")
final <- readRDS("~/Documents/wildfirepm2.5xCOD/data/final_7.01.24.rds")

### Simple model with `fixest`
mod_fixest <-
    feglm(n_deaths ~ 
              popw_mean_pm2.5 + 
              ns(popw_monthly_mean_tempF, df=5) | fipsihme^month + year, 
          offset = log(final$pop),
          data = final,
          weights = final$pop,
          family = quasipoisson,
          cluster = final$fipsihme)

summary(mod_fixest)
interpret_model(mod_fixest, "iid")
interpret_model(mod_fixest, "cluster")


### Simple model with `gnm`
library(gnm)

final_gnm <-
    final %>% 
    unite(county_month, c(fipsihme, month), sep="_")

mod_gnm <-
    gnm(n_deaths ~
            popw_mean_pm2.5 +
            ns(popw_monthly_mean_tempF, df=5) +
            as.factor(year),
        family = quasipoisson,
        eliminate = as.factor(county_month),
        offset = log(final_gnm$pop),
        weights = final_gnm$pop,
        data=final_gnm
    )
summary(mod_gnm)
interpret_model(mod_gnm, "")

### Simple model with `plm` --doesn't have quasipoisson directly, so uses coeftest
library(plm)
library(lmtest) # For clustered standard errors
library(sandwich) # For robust standard errors

final_plm <- pdata.frame(final, index = c("fipsihme", "year"))

plm_model <- plm(n_deaths ~ popw_mean_pm2.5 + ns(popw_monthly_mean_tempF, df=5) + log(pop), 
             data = final_plm, 
             model = "within", 
             effect = "twoways", # This specifies two-way fixed effects
             index = c("fipsihme", "month"))

summary(plm_model)

coeftest(plm_model, vcov = vcovHC(plm_model, type = "HC1", cluster = "group"))

### `lme4`
library(lme4)


### `glmmTMB`
library(glmmTMB)




### Simple model with `glm` #!#!#!#! maxes out memory, can't run glm2, I remember this being an issue before
library(glm2)


# final$fipsihme_factor <- as.factor(final$fipsihme)
# final$month_factor <- as.factor(final$month)
# 
# model <- glm2(n_deaths ~ mean_pm2.5 + fipsihme_factor + month_factor + offset(log(pop)), 
#               data = final, 
#               family = quasipoisson, 
#               weights = final$pop)






