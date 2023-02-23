### Tidy data and run initial model ####

#Tidy final data
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

# Run initial model
mod1 <- feglm(n_deaths ~ 
                  mean_pm2.5 + 
                  ns(temp, df=5) + 
                  precip | yearmonth + fipsihme, 
              offset = log(final$pop),
              data = final,
              weights = final$pop,
              family = quasipoisson,
              cluster = final$fipsihme)

summary(mod1)
interpret_model(mod1)
mod1_FEs <- fixef(mod1)
