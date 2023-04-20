### Working on issue where my wfpm2.5 calculations are different than Childs et al map ####


# smoke PM predictions 
smokePM <- readRDS("data_raw/smokePM_predictions_20060101_20201231.rds")

counties_sf <- 
    #counties(year = 2006) %>% #2006 not working right now, try again later
    counties(year = 2020) %>% 
    filter(STATEFP %in% non_CONUS_FIPS == F) %>% 
    left_join(
        ihme_fips %>% dplyr::select(orig_fips, ihme_fips),
        by=c("COUNTYFP" = "orig_fips")
    )

# read in the grid
grid_10km <- 
    st_read("data_raw/10km_grid/10km_grid_wgs84/10km_grid_wgs84.shp") %>%
    st_transform(st_crs(counties_sf))

county_cross <- readRDS("data/county_xwalk")

# lets only save predictions if there's a smoke day in the unit, start by identifying smoke-days per unit
county_smoke_days <- 
    smokePM %>% # 51434138 rows
    # add unit information, this will duplicate any rows that are in multiple counties
    left_join(county_cross %>% dplyr::select(grid_id_10km, GEOID),
              by = "grid_id_10km") %>% # 76009188 rows for county 
    filter(!is.na(GEOID)) %>% # drop grid cells that don't match to a unit
    # full set of unit-days with smoke 
    dplyr::select(date, GEOID) %>% 
    distinct()  # 2308941 rows (should actually be less after dropping NAs)

county_smokePM <- 
    county_smoke_days %>%
    # join in all grid-cells for each unit
    left_join(county_cross, by = "GEOID") %>% # 119622779 rows
    left_join(smokePM) 


# fill missings with 0s
small <- county_smokePM %>% slice(1:1000)

NOWEIGHT_avg_county_smokePM <- 
    county_smokePM %>% 
    replace_na(list(smokePM_pred = 0)) %>%
    mutate(area2 = unclass(area)) %>%
    group_by(GEOID, date) %>% 
    summarise(smokePM_pred = weighted.mean(smokePM_pred, area2)) %>% 
    ungroup

#note--GEOID is county-specific
saveRDS(NOWEIGHT_avg_county_smokePM, 
        "data/AREAWEIGHT_county_smokePM_predictions_20060101_20201231.rds")

AREAWEIGHT_county_smokePM_features <-
    NOWEIGHT_avg_county_smokePM %>% 
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

# merge with county sf 
fipsihme_sf <- 
    counties(year = 2020) %>% # year should be 2006 once those data are working again
    filter(STATEFP %in% non_CONUS_FIPS == F) %>% 
    dplyr::mutate(COUNTYFP = paste0(STATEFP, COUNTYFP)) %>% 
    dplyr::select(COUNTYFP, NAME) %>% 
    left_join(
        ihme_fips %>% dplyr::select(COUNTYFP = orig_fips, ihme_fips),
        by=c("COUNTYFP")
    ) %>% 
    mutate(
        ihme_fips = ifelse(is.na(ihme_fips), COUNTYFP, ihme_fips)
    ) %>% 
    dplyr::select(fipsihme = ihme_fips, COUNTYFP, NAME)

areaweight_sf <-
    left_join(
        AREAWEIGHT_county_smokePM_features,
        fipsihme_sf %>% rename(GEOID = fipsihme)
    )

# map area-weighted values
ma_smokemap <-
    areaweight_sf %>% 
    group_by(GEOID) %>% 
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
