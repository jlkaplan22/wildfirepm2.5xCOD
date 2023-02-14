#Loading ACS variables:
loadACS <- function(yr, svy) {
    us_ages <-
        get_acs(
            geography = "county",
            table = "B01001",
            year = yr,
            survey = svy
        ) %>%
        left_join(
            load_variables(yr, svy) %>%
                filter(str_detect(name, "B01001")) %>%
                dplyr::select(name, label),
            by=c("variable"="name")
        ) %>%
        dplyr::select(-variable, -moe) %>% 
        pivot_wider(names_from = label, values_from = estimate) %>% 
        group_by(GEOID) %>% 
        mutate(
            .keep=c("unused"),
            pop = `Estimate!!Total`,
            
            male = `Estimate!!Total!!Male`,
            male_u15 = 
                sum(
                    `Estimate!!Total!!Male!!Under 5 years`, 
                    `Estimate!!Total!!Male!!5 to 9 years`,
                    `Estimate!!Total!!Male!!10 to 14 years`
                ),
            male_15to19 = 
                sum(
                    `Estimate!!Total!!Male!!15 to 17 years`, 
                    `Estimate!!Total!!Male!!18 and 19 years`
                ),
            male_20to24 = 
                sum(
                    `Estimate!!Total!!Male!!20 years`,
                    `Estimate!!Total!!Male!!21 years`,
                    `Estimate!!Total!!Male!!22 to 24 years`
                ),
            male_25to29 = `Estimate!!Total!!Male!!25 to 29 years`,
            male_30to34 = `Estimate!!Total!!Male!!30 to 34 years`,
            male_35to39 = `Estimate!!Total!!Male!!35 to 39 years`,
            male_40to44 = `Estimate!!Total!!Male!!40 to 44 years`,
            male_45to49 = `Estimate!!Total!!Male!!45 to 49 years`,
            male_50to54 = `Estimate!!Total!!Male!!50 to 54 years`,
            male_55to59 = `Estimate!!Total!!Male!!55 to 59 years`,
            male_60to64 = 
                sum(
                    `Estimate!!Total!!Male!!60 and 61 years`,
                    `Estimate!!Total!!Male!!62 to 64 years`
                ),
            male_65to69 = 
                sum(
                    `Estimate!!Total!!Male!!65 and 66 years`,
                    `Estimate!!Total!!Male!!67 to 69 years`
                ),
            male_70to74 = `Estimate!!Total!!Male!!70 to 74 years`,
            male_75to79 = `Estimate!!Total!!Male!!75 to 79 years`,
            male_80to84 = `Estimate!!Total!!Male!!80 to 84 years`,
            male_o85 = `Estimate!!Total!!Male!!85 years and over`,
            
            female = `Estimate!!Total!!Female`,
            female_u15 = 
                sum(
                    `Estimate!!Total!!Female!!Under 5 years`, 
                    `Estimate!!Total!!Female!!5 to 9 years`,
                    `Estimate!!Total!!Female!!10 to 14 years`
                ),
            female_15to19 = 
                sum(
                    `Estimate!!Total!!Female!!15 to 17 years`, 
                    `Estimate!!Total!!Female!!18 and 19 years`
                ),
            female_20to24 = 
                sum(
                    `Estimate!!Total!!Female!!20 years`,
                    `Estimate!!Total!!Female!!21 years`,
                    `Estimate!!Total!!Female!!22 to 24 years`
                ),
            female_25to29 = `Estimate!!Total!!Female!!25 to 29 years`,
            female_30to34 = `Estimate!!Total!!Female!!30 to 34 years`,
            female_35to39 = `Estimate!!Total!!Female!!35 to 39 years`,
            female_40to44 = `Estimate!!Total!!Female!!40 to 44 years`,
            female_45to49 = `Estimate!!Total!!Female!!45 to 49 years`,
            female_50to54 = `Estimate!!Total!!Female!!50 to 54 years`,
            female_55to59 = `Estimate!!Total!!Female!!55 to 59 years`,
            female_60to64 = 
                sum(
                    `Estimate!!Total!!Female!!60 and 61 years`,
                    `Estimate!!Total!!Female!!62 to 64 years`
                ),
            female_65to69 = 
                sum(
                    `Estimate!!Total!!Female!!65 and 66 years`,
                    `Estimate!!Total!!Female!!67 to 69 years`
                ),
            female_70to74 = `Estimate!!Total!!Female!!70 to 74 years`,
            female_75to79 = `Estimate!!Total!!Female!!75 to 79 years`,
            female_80to84 = `Estimate!!Total!!Female!!80 to 84 years`,
            female_o85 = `Estimate!!Total!!Female!!85 years and over`,
        ) %>% 
        ungroup()
    
    us_maritalstatus <-
        get_acs(
            geography = "county",
            variables =
                c(
                    tot_male_over15 = "B12002_002",
                    tot_male_never_married = "B12002_003",
                    tot_male_now_married = "B12002_018",
                    tot_male_widowed = "B12002_065",
                    tot_male_divorced = "B12002_080",
                    
                    tot_female_over15 = "B12002_095",
                    tot_female_never_married = "B12002_096",
                    tot_female_now_married = "B12002_111",
                    tot_female_widowed = "B12002_158",
                    tot_female_divorced = "B12002_173"
                ),
            year = yr,
            survey = svy,
            output = "wide"
        )
    
    us <-
        full_join(
            us_ages,
            us_maritalstatus
        ) %>% 
        mutate(year = yr) %>% 
        dplyr::select(GEOID, NAME, year, everything())
    
    return(us)
}

# alternative version for different var naming conventinos
loadACS_alt <- function(yr, svy) {
    us_ages <-
        get_acs(
            geography = "county",
            table = "B01001",
            year = yr, 
            survey = svy
        ) %>%
        left_join(
            load_variables(yr, svy) %>%
                filter(str_detect(name, "B01001")) %>%
                dplyr::select(name, label),
            by=c("variable"="name")
        ) %>%
        dplyr::select(-variable, -moe) %>% 
        pivot_wider(names_from = label, values_from = estimate) %>% 
        group_by(GEOID) %>% 
        mutate(
            .keep=c("unused"),
            pop = `Estimate!!Total:`,
            
            male = `Estimate!!Total:!!Male:`,
            male_u15 = 
                sum(
                    `Estimate!!Total:!!Male:!!Under 5 years`, 
                    `Estimate!!Total:!!Male:!!5 to 9 years`,
                    `Estimate!!Total:!!Male:!!10 to 14 years`
                ),
            male_15to19 = 
                sum(
                    `Estimate!!Total:!!Male:!!15 to 17 years`, 
                    `Estimate!!Total:!!Male:!!18 and 19 years`
                ),
            male_20to24 = 
                sum(
                    `Estimate!!Total:!!Male:!!20 years`,
                    `Estimate!!Total:!!Male:!!21 years`,
                    `Estimate!!Total:!!Male:!!22 to 24 years`
                ),
            male_25to29 = `Estimate!!Total:!!Male:!!25 to 29 years`,
            male_30to34 = `Estimate!!Total:!!Male:!!30 to 34 years`,
            male_35to39 = `Estimate!!Total:!!Male:!!35 to 39 years`,
            male_40to44 = `Estimate!!Total:!!Male:!!40 to 44 years`,
            male_45to49 = `Estimate!!Total:!!Male:!!45 to 49 years`,
            male_50to54 = `Estimate!!Total:!!Male:!!50 to 54 years`,
            male_55to59 = `Estimate!!Total:!!Male:!!55 to 59 years`,
            male_60to64 = 
                sum(
                    `Estimate!!Total:!!Male:!!60 and 61 years`,
                    `Estimate!!Total:!!Male:!!62 to 64 years`
                ),
            male_65to69 = 
                sum(
                    `Estimate!!Total:!!Male:!!65 and 66 years`,
                    `Estimate!!Total:!!Male:!!67 to 69 years`
                ),
            male_70to74 = `Estimate!!Total:!!Male:!!70 to 74 years`,
            male_75to79 = `Estimate!!Total:!!Male:!!75 to 79 years`,
            male_80to84 = `Estimate!!Total:!!Male:!!80 to 84 years`,
            male_o85 = `Estimate!!Total:!!Male:!!85 years and over`,
            
            female = `Estimate!!Total:!!Female:`,
            female_u15 = 
                sum(
                    `Estimate!!Total:!!Female:!!Under 5 years`, 
                    `Estimate!!Total:!!Female:!!5 to 9 years`,
                    `Estimate!!Total:!!Female:!!10 to 14 years`
                ),
            female_15to19 = 
                sum(
                    `Estimate!!Total:!!Female:!!15 to 17 years`, 
                    `Estimate!!Total:!!Female:!!18 and 19 years`
                ),
            female_20to24 = 
                sum(
                    `Estimate!!Total:!!Female:!!20 years`,
                    `Estimate!!Total:!!Female:!!21 years`,
                    `Estimate!!Total:!!Female:!!22 to 24 years`
                ),
            female_25to29 = `Estimate!!Total:!!Female:!!25 to 29 years`,
            female_30to34 = `Estimate!!Total:!!Female:!!30 to 34 years`,
            female_35to39 = `Estimate!!Total:!!Female:!!35 to 39 years`,
            female_40to44 = `Estimate!!Total:!!Female:!!40 to 44 years`,
            female_45to49 = `Estimate!!Total:!!Female:!!45 to 49 years`,
            female_50to54 = `Estimate!!Total:!!Female:!!50 to 54 years`,
            female_55to59 = `Estimate!!Total:!!Female:!!55 to 59 years`,
            female_60to64 = 
                sum(
                    `Estimate!!Total:!!Female:!!60 and 61 years`,
                    `Estimate!!Total:!!Female:!!62 to 64 years`
                ),
            female_65to69 = 
                sum(
                    `Estimate!!Total:!!Female:!!65 and 66 years`,
                    `Estimate!!Total:!!Female:!!67 to 69 years`
                ),
            female_70to74 = `Estimate!!Total:!!Female:!!70 to 74 years`,
            female_75to79 = `Estimate!!Total:!!Female:!!75 to 79 years`,
            female_80to84 = `Estimate!!Total:!!Female:!!80 to 84 years`,
            female_o85 = `Estimate!!Total:!!Female:!!85 years and over`,
        ) %>% 
        ungroup()
    
    us_maritalstatus <-
        get_acs(
            geography = "county",
            variables =
                c(
                    tot_male_over15 = "B12002_002",
                    tot_male_never_married = "B12002_003",
                    tot_male_now_married = "B12002_018",
                    tot_male_widowed = "B12002_065",
                    tot_male_divorced = "B12002_080",
                    
                    tot_female_over15 = "B12002_095",
                    tot_female_never_married = "B12002_096",
                    tot_female_now_married = "B12002_111",
                    tot_female_widowed = "B12002_158",
                    tot_female_divorced = "B12002_173"
                ),
            year = yr,
            survey = svy,
            output = "wide"
        )
    
    us <-
        full_join(
            us_ages,
            us_maritalstatus
        ) %>% 
        mutate(year = yr) %>% 
        dplyr::select(GEOID, NAME, year, everything())
    
    return(us)
}

