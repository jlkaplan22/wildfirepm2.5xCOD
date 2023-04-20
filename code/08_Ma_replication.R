#### Replicate main findings from Ma et al. 2023 preprint ####

# restrict to 2006-2016
ma_final_df <-
    final %>% 
    filter(year <= 2016)

ma_final_df$mean_pm2.5 %>% mean() #.3887, basically same as their reported .39
ma_final_df$mean_pm2.5 %>% max() #70.94726, same as theirs exactly
ma_final_df$n_deaths %>% sum() #27,811,048 which is very close to their 27,812,837

acm_model <- feglm(n_deaths ~ 
                  mean_pm2.5 + 
                  ns(temp, df=5) | yearmonth + fipsihme, 
              offset = log(ma_final_df$pop),
              data = ma_final_df,
              weights = ma_final_df$pop,
              family = quasipoisson,
              cluster = ma_final_df$fipsihme)

summary(acm_model)
summary(acm_model, vcov="iid") #this should replicate Ma et al. results precisely
interpret_model(acm_model, "iid")
interpret_model(acm_model, "cluster") #statistically significant with robust SEs, pt est: .1432 (.00793, .2786)

# expand Ma analysis to 2020--no longer significant
acm_model_2020 <- feglm(n_deaths ~ 
                  mean_pm2.5 + 
                  ns(temp, df=5) | yearmonth + fipsihme, 
              offset = log(final$pop),
              data = final,
              weights = final$pop,
              family = quasipoisson,
              cluster = final$fipsihme)

summary(acm_model_2020)
summary(acm_model_2020, vcov="iid") 

interpret_model(acm_model_2020, "iid") # no longer significant even with IID SEs

