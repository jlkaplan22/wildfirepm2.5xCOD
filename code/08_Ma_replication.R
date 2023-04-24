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

# Create supplementary table to summarise results
value_replications <-
    cbind(
        #row names
        c(
            "Mean county-month smoke PM2.5 (µg/m3)",
            "Max county-month smomke PM2.5 (µg/m3)",
            "Total deaths",
            "Pt. est. and 95% CI using IID SEs (% change)",
            "Pt. est. and 95% CI using robust SEs (% change)"
        ),
        #values from Ma et al. (2006-2016)
        c(
            "0.39", 
            "70.95", 
            "27,812,837", 
            ".14 (.11, .17)",
            "--"
        ), 
        #replicated values (2006-2016)
        c(
            ma_final_df$mean_pm2.5 %>% mean() %>% round(digits = 3) %>% format(big.mark = ","),
            ma_final_df$mean_pm2.5 %>% max() %>% round(digits = 3) %>% format(big.mark = ","),
            ma_final_df$n_deaths %>% sum() %>% round(digits = 3) %>% format(big.mark = ","),
            interpret_model(acm_model, "iid") %>% str_sub(12),
            interpret_model(acm_model, "cluster") %>% str_sub(12)
        ),
        #replicated values (2006-2020)
        c(
            final$mean_pm2.5 %>% mean() %>% round(digits = 3) %>% format(big.mark = ","),
            final$mean_pm2.5 %>% max() %>% round(digits = 3) %>% format(big.mark = ","),
            final$n_deaths %>% sum() %>% round(digits = 3) %>% format(big.mark = ","),
            interpret_model(acm_model_2020, "iid") %>% str_sub(12),
            interpret_model(acm_model_2020, "cluster") %>% str_sub(12)
        )
    ) %>% 
    data.frame()

colnames(value_replications) <- c(" ", "Ma et al. (2006-2016)", "Replicated values (2006-2016)", "Extension of replication (2006-2020)")

table3 <-
    value_replications %>%
    gt() %>% 
    cols_align(align="center") %>% 
    tab_header(
        title = "Table 3: Ma et al. 2023 (preprint) Replication"
    ) 

#May get a bug on this line--need to restart R in order for it to work, see
#this page: https://stackoverflow.com/questions/73964325/r-gt-gtsave-error-in-sclose-attempt-to-apply-non-function
gtsave(table3, "plots/table3.png")







