### Load ACS variables ###
tic()
acs2009 <- loadACS(2009, "acs5")
toc()

acs2010 <- loadACS(2010, "acs5")
acs2011 <- loadACS(2011, "acs5")
acs2012 <- loadACS(2012, "acs5")
acs2013 <- loadACS(2013, "acs5")
acs2014 <- loadACS(2014, "acs5")
acs2015 <- loadACS(2015, "acs5")

acs2016 <- loadACS(2016, "acs5")
acs2017 <- loadACS(2017, "acs5")
acs2018 <- loadACS(2018, "acs5")
acs2019 <- loadACS(2019, "acs5")
acs2020 <- loadACS(2020, "acs5")

beep()

acs2009to2020 <-
    rbind(
        acs2009,
        acs2010,
        acs2011,
        acs2012,
        acs2013,
        acs2014,
        acs2015,
        acs2016,
        acs2017,
        acs2018,
        acs2019,
        acs2020
    ) %>% 
    select(GEOID, NAME, year, everything()) %>% 
    mutate(
        marital_status =
            case_when(
                marital_status == "all" ~ "all",
                marital_status == "Divorced" ~ "divorced",
                marital_status == "Now married" ~ "married",
                marital_status == "Never married" ~ "single_never_married",
                marital_status == "Widowed" ~ "widowed"
            ),
        age_group2 =
            case_when(
                age_group == "all" ~ "all",
                age_group == "15to19" | age_group == "20to24" |
                    age_group == "25to29" | age_group == "30to34" |
                    age_group == "35to39" | age_group == "40to44" |
                    age_group == "45to49" | age_group == "50to54" |
                    age_group == "55to59" | age_group == "60to64" ~ "under_65",
                age_group == "65to74" | age_group == "75to84" | age_group == "85_and_up" ~ "over_65"
            )
    )

write_csv(acs2009to2020, "data/acs2009to2020.csv.gz")

acs2009to2020 <- read_csv(paste0(data_dir, "acs2009to2020.csv.gz"))
