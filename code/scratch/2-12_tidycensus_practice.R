### playing around with tidycensus and trying ot find marital status data #####

vars2017 <- load_variables(2017, "acs5")

marriage_vars2017 <-
    vars2017 %>% 
    filter(str_detect(concept %>% str_to_lower, "marital status") & geography == "tract")

B12002 <-
    vars2017 %>% 
    filter(str_detect(name, "B12002"))

# mn <-
#     get_acs(
#         geography = "county",
#         variables =
#             c(
#                 pop = "B00001_001",
#                 male_15to17 = "B01001_006",
#                 male_18to19 = "B01001_007",
#                 male_20 = "B01001_008",
#                 male_21 = "B01001_009",
#                 
#                     
#                 tot_male = "B12002_002",
#                 tot_male_never_married = "B12002_003",
#                 tot_male_now_married = "B12002_018",
#                 tot_male_widowed = "B12002_065",
#                 tot_male_divorced = "B12002_080",
# 
#                 tot_female = "B12002_095",
#                 tot_female_never_married = "B12002_096",
#                 tot_female_now_married = "B12002_111",
#                 tot_female_widowed = "B12002_158",
#                 tot_female_divorced = "B12002_173"
#             ),
#         state = "MN",
#         year = 2017
#     )

mn_maritalstatus17 <-
    get_acs(
        geography = "county",
        table = "B12002",
        state = "MN",
        year = 2017
    ) %>%
    left_join(
        load_variables(2017, "acs5") %>%
            filter(str_detect(name, "B12002")) %>%
            dplyr::select(name, label),
        by=c("variable"="name")
    )

mn_ages2006 <-
    get_acs(
        geography = "county",
        table = "B01001",
        state = "MN",
        year = 2006,
        survey = "acs1"
    ) %>%
    left_join(
        load_variables(2006, "acs1") %>%
            filter(str_detect(name, "B01001")) %>%
            dplyr::select(name, label),
        by=c("variable"="name")
    )


mn_ages2019 <-
    get_acs(
        geography = "county",
        table = "B01001",
        state = "MN",
        year = 2019,
    ) %>%
    left_join(
        load_variables(2019, "acs5") %>%
            filter(str_detect(name, "B01001")) %>%
            dplyr::select(name, label),
        by=c("variable"="name")
    )

mn_ages2020 <-
    get_acs(
        geography = "county",
        table = "B01001",
        state = "MN",
        year = 2020,
    ) %>%
    left_join(
        load_variables(2020, "acs5") %>%
            filter(str_detect(name, "B01001")) %>%
            dplyr::select(name, label),
        by=c("variable"="name")
    )

mn_ages17_wide <-
    # get_acs(
    #     geography = "county",
    #     table = "B01001",
    #     state = "MN",
    #     year = 2017
    # ) %>%
    # left_join(
    #     load_variables(2017, "acs5") %>%
    #         filter(str_detect(name, "B01001")) %>%
    #         dplyr::select(name, label),
    #     by=c("variable"="name")
    # ) %>% 
    mn_ages17 %>% 
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

mn_maritalstatus_limited <-
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
        state = "MN",
        year = 2017,
        output = "wide"
    )

mn <-
    full_join(
        mn_ages17_wide,
        mn_maritalstatus_limited
    )

#### pull all together for national data    ####
us_ages_2017 <-
    get_acs(
        geography = "county",
        table = "B01001",
        year = 2017
    ) %>%
    left_join(
        load_variables(2017, "acs5") %>%
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

us_maritalstatus_2017 <-
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
        year = 2017,
        output = "wide"
    )

us_2017 <-
    full_join(
        us_ages_2017,
        us_maritalstatus_2017
    ) %>% 
    mutate(year = 2017) %>% 
    dplyr::select(GEOID, NAME, year, everything())
