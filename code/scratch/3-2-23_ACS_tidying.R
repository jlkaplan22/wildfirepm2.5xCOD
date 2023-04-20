#### Getting ACS data in actual tidy format for Matt, on 3/2/23 ####
source("code/00_config.R")

yr <- 2019
svy <- "acs5"

acs_temp <- 
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
    )

acs_temp_sep <-
    acs_temp %>% 
    separate(
        label,
        into = c("type", "group", "sex", "age_group_raw"),
        sep = "!!"
    ) %>% 
    mutate(
        sex = ifelse(is.na(sex), "all", sex),
        age_group_raw = ifelse(is.na(age_group_raw), "all", age_group_raw),
    ) %>% 
    mutate(
        age_group =
            case_when(
                age_group_raw == "all" ~ "all",
                age_group_raw == "Under 5 years" | 
                    age_group_raw == "5 to 9 years" | 
                    age_group_raw == "10 to 14 years" ~ "u15",
                age_group_raw == "15 to 17 years" | 
                    age_group_raw == "18 and 19 years" ~ "15to19",
                age_group_raw == "20 years" |
                    age_group_raw == "21 years" |
                    age_group_raw == "22 to 24 years" ~ "20to24",
                age_group_raw == "25 to 29 years" ~ "25to29",
                age_group_raw == "30 to 34 years" ~ "30to34",
                age_group_raw == "35 to 39 years" ~ "35to39",
                age_group_raw == "40 to 44 years" ~ "40to44",
                age_group_raw == "45 to 49 years" ~ "45to49",
                age_group_raw == "50 to 54 years" ~ "50to54",
                age_group_raw == "55 to 59 years" ~ "55to59",
                age_group_raw == "60 and 61 years" |
                    age_group_raw == "62 to 64 years" ~ "60to64",
                age_group_raw == "65 and 66 years" |
                    age_group_raw == "67 to 69 years" |
                    age_group_raw == "70 to 74 years" ~ "65to74",
                age_group_raw == "75 to 79 years" |
                    age_group_raw == "80 to 84 years" ~ "75to84",
                age_group_raw == "85 years and over" ~ "o85"
            ) 
    ) %>% 
    dplyr::select(-type, -group, -age_group_raw, -variable, pop = estimate)

acs_temp_new_age_groups <-
    acs_temp_sep %>% 
    group_by(GEOID, NAME, sex, age_group) %>%
    summarise(pop = sum(pop))

# clean up marital status data
acs_marital_status_intermediate <-
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
    ) 

acs_marital_status <-
    acs_marital_status_intermediate %>% 
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
                age_group == "all" ~ "all",
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
                age_group == "85 years and over" ~ "o85"
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
           ) #remove duplicate "all"s that come from specifics of marital status subgroups (eg separated vs not)

    #unite(marital_status, c(marital_status, married_presence_or_absent, married_spouse_absent_reason), sep = "-", na.rm = TRUE)
    

acs_marital_status_final <-
    acs_marital_status %>% 
    group_by(GEOID, NAME, sex, marital_status, age_group) %>%
    summarise(estimate = sum(estimate))
    

merged_acs <-
    full_join(
        acs_temp_new_age_groups,
        acs_marital_status_final
    ) %>% 
    dplyr::select(GEOID, NAME, sex, marital_status, age_group, pop, estimate)



###### OLD FUNCTIONS #######
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
    
#### alternative ACS load for different var naming conventions: ####
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

    
#3-6-23 revisit: is it necessary to have the B01001 data at all?
mini_B01001 <- 
    acs_temp_new_age_groups %>% 
    filter(GEOID == "01001") %>% 
    filter(age_group != "u15") # so we can make direct comparisons

mini_B12002 <-
    acs_marital_status_final %>% 
    filter(GEOID == "01001")

mini_both <-
    full_join(
        mini_B01001,
        mini_B12002
    ) %>% 
    dplyr::select(GEOID, NAME, sex, marital_status, age_group, pop, estimate)

