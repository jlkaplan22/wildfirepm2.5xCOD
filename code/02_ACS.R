### Load ACS variables ###

acs2009to2020 <- #should take ~136 sec
    rbind(
        loadACS(2009, "acs5"),
        loadACS(2010, "acs5"),
        loadACS(2011, "acs5"),
        loadACS(2012, "acs5"),
        loadACS(2013, "acs5"),
        loadACS(2014, "acs5"),
        loadACS(2015, "acs5"),
        loadACS(2016, "acs5"),
        loadACS(2017, "acs5"),
        loadACS(2018, "acs5"),
        loadACS_alt(2019, "acs5"),
        loadACS_alt(2020, "acs5")
    )

write.csv(acs2009to2020, "data/acs2009to2020.csv", row.names = FALSE)

acs2009to2020 <- read_csv("data/acs2009to2020.csv")
