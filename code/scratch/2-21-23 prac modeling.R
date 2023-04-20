### 2-21-23 attempting basic TWFEs ###

source("code/00_config.R")
PRISM <- read_csv("data/prism_monthly.csv")
avg_county_smokePM <- readRDS("data/county_smokePM_predictions_20060101_20201231.rds")
county_smokePM_features <-
    avg_county_smokePM %>% 
    mutate(year = year(date), month = month(date)) %>% 
    group_by(GEOID, year, month) %>% 
    summarise(
        mean_pm2.5 = mean(smokePM_pred),
        cum_pm2.5 = sum(smokePM_pred),
        daysover0 = sum(smokePM_pred > 0),
        daysover5 = sum(smokePM_pred > 5),
        daysover12.5 = sum(smokePM_pred > 12.5),
        daysover20 = sum(smokePM_pred > 20),
        daysover40 = sum(smokePM_pred > 40)
    )


# Load COD data
COD_raw <- readRDS("data_raw/age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-all_cause.RDS")

# Visualize all subgroups
COD_raw %>% 
    dplyr::select(death_type, race_eth, age_group, sex, marital) %>% 
    distinct() %>% 
    print(n = 24)

# Remove subgroupings and aggregate to total 
COD <- 
    COD_raw %>% 
    mutate(statefips = str_sub(fipsihme, 1, 2)) %>% 
    filter(statefips %in% non_CONUS_FIPS == FALSE) %>% 
    filter(
        year >= 2006,
        race_eth == "all" & 
        age_group == "all_ages" & 
        sex == "all"
    ) %>% 
    group_by(fipsihme, year, month) %>% 
    summarise(pop = sum(pop), n_deaths = sum(n_deaths), rate = n_deaths / pop) %>% 
    ungroup()

# Sanity check to make sure pop numbers look right
COD %>% 
    group_by(year) %>% 
    summarise(n_deaths = sum(n_deaths))

#### 2-22-23 Spot-checking COD data ####
#SF county
COD %>% 
    filter(fipsihme == "06075") %>% 
    group_by(year, pop) %>% 
    summarise(n_deaths = sum(n_deaths))

COD %>% 
    filter(fipsihme == "06075") %>% 
    filter(year == 2014)

#Hennepin county
COD %>% 
    filter(fipsihme == "27053") %>% 
    group_by(year, pop) %>% 
    summarise(n_deaths = sum(n_deaths))

COD %>% 
    filter(fipsihme == "27053") %>% 
    filter(year == 2009)

#Steuben county, NY
COD %>% 
    filter(fipsihme == "36101") %>% 
    group_by(year, pop) %>% 
    summarise(n_deaths = sum(n_deaths))

COD %>% 
    filter(fipsihme == "36101") %>% 
    filter(year == 2016)

#look at cardiovascular deaths
cvd_raw <- readRDS("data_raw/age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-cardiovascular.RDS")
cvd_raw %>% 
    dplyr::select(death_type, race_eth, age_group, sex, marital) %>% 
    distinct() %>% 
    print(n = 24)
cvd <- 
    cvd_raw %>% 
    mutate(statefips = str_sub(fipsihme, 1, 2)) %>% 
    filter(statefips %in% non_CONUS_FIPS == FALSE) %>% 
    filter(
        year >= 2006,
        race_eth == "all" & 
            age_group == "all_ages" & 
            sex == "all"
    ) %>% 
    group_by(fipsihme, year, month) %>% 
    summarise(pop = sum(pop), n_deaths = sum(n_deaths), rate = n_deaths / pop) %>% 
    ungroup()

#look at raw respiratory deaths
resp_raw <- readRDS("data_raw/age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-respiratory.RDS")
resp_raw %>% 
    dplyr::select(death_type, race_eth, age_group, sex, marital) %>% 
    distinct() %>% 
    print(n = 24)
resp <- 
    resp_raw %>% 
    mutate(statefips = str_sub(fipsihme, 1, 2)) %>% 
    filter(statefips %in% non_CONUS_FIPS == FALSE) %>% 
    filter(
        year >= 2006,
        race_eth == "all" & 
            age_group == "all_ages" & 
            sex == "all"
    ) %>% 
    group_by(fipsihme, year, month) %>% 
    summarise(pop = sum(pop), n_deaths = sum(n_deaths), rate = n_deaths / pop) %>% 
    ungroup()

#look at raw neuropsych / mental disorder deaths:
neuropsych_raw <- readRDS("data_raw/age_standardized_rates/age_standardized_rates-county_month-all_sex-all_marital-neuropsychiatric.RDS")
neuropsych_raw %>% 
    dplyr::select(death_type, race_eth, age_group, sex, marital) %>% 
    distinct() %>% 
    print(n = 24)
neuropsych <- 
    neuropsych_raw %>% 
    mutate(statefips = str_sub(fipsihme, 1, 2)) %>% 
    filter(statefips %in% non_CONUS_FIPS == FALSE) %>% 
    filter(
        year >= 2006,
        race_eth == "all" & 
            age_group == "all_ages" & 
            sex == "all"
    ) %>% 
    group_by(fipsihme, year, month) %>% 
    summarise(pop = sum(pop), n_deaths = sum(n_deaths), rate = n_deaths / pop) %>% 
    ungroup()


#Overall CVD deaths:
cvd %>% 
    group_by(year) %>% 
    summarise(n_deaths = sum(n_deaths), pop = sum(pop))

#San Francisco county
cvd %>% 
    filter(fipsihme == "06075") %>% 
    filter(year == 2014)

#### Match Ma et al summary death numbers ####
COD %>% 
    filter(year >= 2006 & year <= 2016) %>% 
    summarise(n_deaths = sum(n_deaths)) #Total deaths: 27811048

cvd %>% 
    filter(year >= 2006 & year <= 2016) %>% 
    summarise(n_deaths = sum(n_deaths)) #Total deaths: 8825700

resp %>% 
    filter(year >= 2006 & year <= 2016) %>% 
    summarise(n_deaths = sum(n_deaths)) #Total deaths: 2713414

neuropsych %>% 
    filter(year >= 2006 & year <= 2016) %>% 
    summarise(n_deaths = sum(n_deaths)) #Total deaths: 3225010

#### Match Ma et al. PM2.5 numbers ####
final %>% 
    dplyr::pull(mean_pm2.5) %>% 
    mean() #1.610783

#max mean pm2.5 matches what Ma et al reports, exactly
final %>% 
    filter(mean_pm2.5 == max(final$mean_pm2.5))

#replicate their all-year monthly mean wfpm2.5 map
ma_smokemap <-
    final %>% 
    group_by(fipsihme) %>% 
    summarise(mean_pm2.5 = mean(mean_pm2.5)) %>% 
    mutate(
        smokebins = cut(mean_pm2.5, breaks = c(.06, .3, .4, .5, 1.86, Inf))
    ) %>% 
    left_join(
        fipsihme_sf %>% dplyr::select(fipsihme, geometry)
    ) %>% 
    st_as_sf() %>% #key step to recover sf geometry for some reason
    ggplot(aes(fill = smokebins)) +
    geom_sf(color = NA) + #color=NA to remove county borders
    scale_fill_brewer(palette = "Reds") +
    ggtitle("Mean wfpm2.5 across all year-months, 2006-2016")


ggsave(filename = "plots/ma_smokemap_replication.jpg", plot = ma_smokemap, device = "jpeg", dpi = 100)

#replicate Childs et al map
childs_smokemap2013 <-
    final %>% 
    filter(year == 2013) %>% 
    group_by(fipsihme) %>% 
    summarise(mean_pm2.5 = mean(mean_pm2.5)) %>% 
    left_join(
        fipsihme_sf %>% dplyr::select(fipsihme, geometry)
    ) %>% 
    st_as_sf() %>% #key step to recover sf geometry for some reason
    ggplot(aes(fill = mean_pm2.5)) +
    geom_sf(color = NA) + #color=NA to remove county borders
    theme_minimal() +
    scale_fill_viridis(option="magma") +
    ggtitle("Mean wfpm2.5 across all months, 2013")


ggsave(filename = "plots/childs_smokemap_rep_2013.jpg", plot = childs_smokemap2013, device = "jpeg", dpi = 100)

childs_smokemap2007 <-
    final %>% 
    filter(year == 2007) %>% 
    group_by(fipsihme) %>% 
    summarise(mean_pm2.5 = mean(mean_pm2.5)) %>% 
    left_join(
        fipsihme_sf %>% dplyr::select(fipsihme, geometry)
    ) %>% 
    st_as_sf() %>% #key step to recover sf geometry for some reason
    ggplot(aes(fill = mean_pm2.5)) +
    geom_sf(color = NA) + #color=NA to remove county borders
    theme_minimal() +
    scale_fill_viridis(option="magma") +
    ggtitle("Mean wfpm2.5 across all months, 2007")


ggsave(filename = "plots/childs_smokemap_rep_2007.jpg", plot = childs_smokemap2007, device = "jpeg", dpi = 100)

childs_smokemap2006 <-
    final %>% 
    filter(year == 2006) %>% 
    group_by(fipsihme) %>% 
    summarise(mean_pm2.5 = mean(mean_pm2.5)) %>% 
    left_join(
        fipsihme_sf %>% dplyr::select(fipsihme, geometry)
    ) %>% 
    st_as_sf() %>% #key step to recover sf geometry for some reason
    ggplot(aes(fill = mean_pm2.5)) +
    geom_sf(color = NA) + #color=NA to remove county borders
    theme_minimal() +
    scale_fill_viridis(option="magma") +
    ggtitle("Mean wfpm2.5 across all months, 2006")


ggsave(filename = "plots/childs_smokemap_rep_2006.jpg", plot = childs_smokemap2006, device = "jpeg", dpi = 100)


# Tidy final data #!#!#!#! missing some PRISM values (4,368) that I need to take care of
final <-
    COD %>% 
    filter(year >= 2006 & year <= 2016) %>% 
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

# fixest Poisson TWFE
mean(final$n_deaths) #71.233
var(final$n_deaths) #37603.29, so data are highly overdispersed

subset <-
    final %>% 
    filter(!is.na(temp)) %>% 
    filter(fipsihme == "06037") %>% 
    filter(year == 2010)

splinetest <- feglm(n_deaths ~ 
                        ns(temp, df=5) | year + fipsihme, 
                    offset = log(subset$pop),
                    data = subset,
                    family = quasipoisson)

splinetest <- feglm(n_deaths ~ 
                        ns(temp, df=3), 
                    data = subset)

plot(final$temp, 
     final$n_deaths)

plot(final %>% filter(!is.na(temp)) %>% pull(temp), 
      fitted(splinetest),col=2)

plot(subset$temp, subset %>% filter(!is.na(temp)) %>% pull(n_deaths), col=1)
points(subset$temp, fitted(splinetest), col=2, add=T)
lines(subset$temp, fitted(splinetest), col=2)

mod1 <- feglm(n_deaths ~ 
                   1 + mean_pm2.5 + 
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

# try with glm instead #!#!#! too inefficient, exhausts memory
# mod2 <- glm(n_deaths ~ 
#                   mean_pm2.5 + 
#                   ns(temp, df=5) + 
#                   precip + as.factor(year) + as.factor(fipsihme), 
#               offset = log(final$pop),
#               data = final,
#               weights = final$pop,
#               family = quasipoisson)
# summary(mod2)

# use `dlnm` package? (as was done in which paper--need to find htis)



