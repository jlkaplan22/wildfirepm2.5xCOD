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
        acs2009 %>% mutate(year = 2009),
        acs2010 %>% mutate(year = 2010),
        acs2011 %>% mutate(year = 2011),
        acs2012 %>% mutate(year = 2012),
        acs2013 %>% mutate(year = 2013),
        acs2014 %>% mutate(year = 2014),
        acs2015 %>% mutate(year = 2015),
        acs2016 %>% mutate(year = 2016),
        acs2017 %>% mutate(year = 2017),
        acs2018 %>% mutate(year = 2018),
        acs2019 %>% mutate(year = 2019),
        acs2020 %>% mutate(year = 2020),
    ) %>% 
    select(GEOID, NAME, year, everything())

write_csv(acs2009to2020, "data/acs2009to2020.csv.gz")

acs2009to2020 <- read_csv(paste0(data_dir, "acs2009to2020.csv"))

