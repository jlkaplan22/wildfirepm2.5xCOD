#### Loading ACS variables: ####
loadACS <- function(yr, svy) {
    acs_marital_status <-
        get_acs(
            geography = "county",
            table = "B12002",
            year = yr,
            survey = svy,
            cache_table = TRUE
        ) %>%
        left_join(
            load_variables(yr, svy) %>%
                filter(str_detect(name, "B12002")) %>%
                dplyr::select(name, label),
            by=c("variable"="name")
        ) %>% 
        separate(
            label,
            into = c("x1", "x2", "sex", "marital_status", "x5", "x6", "x7"), 
            sep = "!!",
            remove = FALSE
        ) %>% 
        mutate(
            age_group = 
                case_when(
                    str_detect(x5, "[0-9]") ~ x5,
                    str_detect(x6, "[0-9]") ~ x6,
                    str_detect(x7, "[0-9]") ~ x7,
                ),
            married_presence_or_absent = 
                case_when(
                    x5 == "Married, spouse present" ~ "present",
                    x5 == "Married, spouse absent" ~ "absent",
                    TRUE ~ NA_character_
                ),
            married_spouse_absent_reason =
                case_when(
                    x6 == "Separated" ~ "separated",
                    x6 == "Other" ~ "other",
                    TRUE ~ NA_character_
                )
        ) %>% 
        mutate(
            marital_status = ifelse(is.na(marital_status), "all", marital_status),
            sex = ifelse(is.na(sex), "all", sex),
            age_group = ifelse(is.na(age_group), "all", age_group)
        ) %>% 
        mutate(
            age_group =
                case_when(
                    age_group == "all" ~ "15_and_up",
                    age_group == "15 to 17 years" | 
                        age_group == "18 and 19 years" ~ "15to19",
                    age_group == "20 to 24 years" ~ "20to24",
                    age_group == "25 to 29 years" ~ "25to29",
                    age_group == "30 to 34 years" ~ "30to34",
                    age_group == "35 to 39 years" ~ "35to39",
                    age_group == "40 to 44 years" ~ "40to44",
                    age_group == "45 to 49 years" ~ "45to49",
                    age_group == "50 to 54 years" ~ "50to54",
                    age_group == "55 to 59 years" ~ "55to59",
                    age_group == "60 to 64 years" ~ "60to64",
                    age_group == "65 to 74 years" ~ "65to74",
                    age_group == "75 to 84 years" ~ "75to84",
                    age_group == "85 years and over" ~ "85_and_up"
                )
        ) %>% 
        dplyr::select(-x1, -x2, -x5, -x6, -x7, -label) %>% 
        filter(variable != "B12002_019", 
               variable != "B12002_034",
               variable != "B12002_035",
               variable != "B12002_050",
               
               variable != "B12002_112",
               variable != "B12002_127",
               variable != "B12002_128",
               variable != "B12002_143"
        ) %>% #remove duplicate "all"s that come from specifics of marital status subgroups (eg separated vs not)
        group_by(GEOID, NAME, sex, marital_status, age_group) %>%
        summarise(estimate = sum(estimate))
    
    #clean up some formatting
    acs_marital_status <-
        acs_marital_status %>% 
        mutate(
            sex = 
                case_when(
                    sex == "Female:" ~ "Female",
                    sex == "Male:" ~ "Male",
                    TRUE ~ sex
                ),
            marital_status = 
                case_when(
                    marital_status == "Divorced:" ~ "Divorced",
                    marital_status == "Never married:" ~ "Never married",
                    marital_status == "Now married:" ~ "Now married",
                    marital_status == "Widowed:" ~ "Widowed",
                    TRUE ~ marital_status
                ),
            year = yr
        )
    
    ## the "all" for sex and marital_status doesn't include every combo so 
    ## remove and reconstruct them
    pop_df <- 
        acs_marital_status %>% 
        filter(sex != "all",
               marital_status != "all")
    
    ## Make a set of "all" sex
    all_sex <- pop_df %>% 
        mutate(sex = "all") %>% 
        group_by(GEOID, NAME, year, sex, marital_status, age_group) %>% 
        summarize(estimate = sum(estimate))
    
    all_marital <- pop_df %>% 
        mutate(marital_status = "all") %>% 
        group_by(GEOID, NAME, year, sex, marital_status, age_group) %>% 
        summarize(estimate = sum(estimate))
    
    all_sex_all_marital <- pop_df %>% 
        mutate(sex = "all",
               marital_status = "all") %>% 
        group_by(GEOID, NAME, year, sex, marital_status, age_group) %>% 
        summarize(estimate = sum(estimate))
    
    marital_population <- 
        bind_rows(
            pop_df,
            all_sex,
            all_marital,
            all_sex_all_marital
        ) %>% 
        ungroup() %>% 
        arrange(GEOID, year, sex, marital_status, age_group)
    
    return(marital_population)
}


#### Interpret model coefs ####
interpret_model <- function(model, se_type) {
    sig_digits <- 4
    if (se_type == "cluster") {
        wfpm2.5_beta <- coefficients(model, vcov_cluster = "fipsihme")[1] %>% as.numeric()
        wfpm2.5_se <- fixest::se(model, vcov_cluster = "fipsihme")[1] %>% as.numeric()
    } else if (se_type == "hetero") {
        wfpm2.5_beta <- coefficients(model, vcov = se_type)[1] %>% as.numeric()
        wfpm2.5_se <- fixest::se(model, vcov = se_type)[1] %>% as.numeric()
    } else if (se_type == "iid") {
        wfpm2.5_beta <- coefficients(model, vcov = se_type)[1] %>% as.numeric()
        wfpm2.5_se <- fixest::se(model, vcov = se_type)[1] %>% as.numeric()
    } else { #this basically just covers the `gnm` output
        wfpm2.5_beta <- coefficients(model)[1] %>% as.numeric()
        wfpm2.5_se <- gnm::se(model)[1,2] %>% as.numeric()
    }
    
    CI_lower = exp(wfpm2.5_beta - 1.96 * wfpm2.5_se)
    CI_upper = exp(wfpm2.5_beta + 1.96 * wfpm2.5_se) 
    
    percent_change = (exp(wfpm2.5_beta) - 1) * 100 
    per_change_CI_lower = (CI_lower - 1) * 100 
    per_change_CI_upper = (CI_upper - 1) * 100 
    
    return(
        paste0(
            "% change = ", percent_change %>% formatC(digits = sig_digits), " (", 
            per_change_CI_lower %>% formatC(digits = sig_digits), ", ", 
            per_change_CI_upper %>% formatC(digits = sig_digits), ")"
        )
    )
}

#### Return TWFE model using standard params, useful for sub-analyses ####
subanalysis_modeler <- function(df) {
    return(
        feglm(n_deaths ~ 
                  mean_pm2.5 + 
                  ns(weighted_temp, df=3) + weighted_precip | fipsihme^month + year, 
              offset = log(df$pop),
              data = df,
              weights = df$pop,
              family = quasipoisson,
              cluster = df$fipsihme)
    )
}

#### Return TWFE with specified params ####
modeler <- function(df, x, ctrls, fes, cluster) {
    ## define the formula for the model
    ctrls <- ifelse(ctrls != "", paste0("+", ctrls), ctrls) #need to add plus if there are controls
    formula <- paste0(x, ctrls, "|", fes)
    
    mod <-
        feglm(
            as.formula(
                paste0("n_deaths", "~", formula)
            ),
            offset = log(df$pop),
            data = df,
            weights = df$pop,
            family = quasipoisson,
            cluster = cluster)
    
    mean_pm2.5 <- coefficients(mod)[1]
    se_county_cluster <- fixest::se(mod)[1]
    pval_county_cluster <- summary(mod)$coeftable[1,4]
    se_iid <- fixest::se(mod, vcov="iid")[1]
    pval_iid <- summary(mod, vcov="iid")$coeftable[1,4]
    se_hetero <- fixest::se(mod, vcov="hetero")[1]
    pval_hetero <- summary(mod, vcov="hetero")$coeftable[1,4]

    row <- 
        c(mean_pm2.5, se_county_cluster, pval_county_cluster, 
          se_iid, pval_iid, 
          se_hetero, pval_hetero) %>%
        t() %>% 
        data.frame()
    
    colnames(row) <- c("mean_pm2.5", "se_county_cluster", "pval_county_cluster",
                       "se_iid", "pval_iid",
                       "se_hetero", "pval_hetero")
    
    return(row)
}

#### Return TWFE with specified params, adding weighting as option ####
modeler_weighted <- function(df, x, ctrls, fes, cluster, weights) {
    ## define the formula for the model
    ctrls <- ifelse(ctrls != "", paste0("+", ctrls), ctrls) #need to add plus if there are controls
    formula <- paste0(x, ctrls, "|", fes)
    
    if (weights == TRUE) {
        mod <-
            feglm(
                as.formula(
                    paste0("n_deaths", "~", formula)
                ),
                offset = log(df$pop),
                data = df,
                weights = df$pop,
                family = quasipoisson,
                cluster = cluster)
    } else {
        mod <-
            feglm(
                as.formula(
                    paste0("n_deaths", "~", formula)
                ),
                offset = log(df$pop),
                data = df,
                family = quasipoisson,
                cluster = cluster)
    }
    
    mean_pm2.5 <- coefficients(mod)[1]
    se_county_cluster <- fixest::se(mod)[1]
    pval_county_cluster <- summary(mod)$coeftable[1,4]
    se_iid <- fixest::se(mod, vcov="iid")[1]
    pval_iid <- summary(mod, vcov="iid")$coeftable[1,4]
    se_hetero <- fixest::se(mod, vcov="hetero")[1]
    pval_hetero <- summary(mod, vcov="hetero")$coeftable[1,4]
    
    row <- 
        c(mean_pm2.5, se_county_cluster, pval_county_cluster, 
          se_iid, pval_iid, 
          se_hetero, pval_hetero) %>%
        t() %>% 
        data.frame()
    
    colnames(row) <- c("mean_pm2.5", "se_county_cluster", "pval_county_cluster",
                       "se_iid", "pval_iid",
                       "se_hetero", "pval_hetero")
    
    return(row)
}

#### PRISM data download helper ####
prism_helper <- function(year, env_var) {
    year_abb = str_sub(year, 3, 4)
    
    if (env_var == "temp") {
        prism_call <- "tmean"
    } else if (env_var == "precip") {
        prism_call <- "ppt"
    }
    
    options(prism.path=paste0(raw_data_dir, "prism/", env_var, "_dailys/", env_var, year_abb))
    get_prism_dailys(type=prism_call, minDate = paste0(year, "-01-01"), maxDate = paste0(year, "-12-31"), keepZip=FALSE)
    
    return(pd_stack(prism_archive_ls()))
}
