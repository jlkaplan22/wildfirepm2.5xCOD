# ------------------------------------------------------------------------------
# Written by: Marissa Childs (from Childs et al. 2022), modified by Jordan Kaplan
# Aggregates 10 km grid smokePM predictions to county level.
# ------------------------------------------------------------------------------

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
    st_read(paste0(raw_data_dir, "10km_grid/10km_grid_wgs84/10km_grid_wgs84.shp")) %>% 
    st_transform(st_crs(counties_sf))

# make a crosswalk with intersection area with grid cells
sf_use_s2(FALSE)
county_cross <-
    st_intersection(counties_sf, grid_10km) %>%
    dplyr::select(GEOID, grid_id_10km = ID) %>%
    mutate(area=st_area(.)) %>%
    st_drop_geometry()

# save the crosswalk since it takes a while to make 
saveRDS(county_cross, "data/county_xwalk")

# Shortcut for once file has been saved
#county_cross <- readRDS("data/county_xwalk")


# population by grid cell
pop <-
    list.files("data_raw/populationDensity_10km_subgrid", full.names = TRUE) %>% 
    purrr::map_dfr(read_csv)

# smoke PM predictions 
smokePM <- readRDS("data_raw/smokePM_predictions_20060101_20201231.rds")

# only save predictions if there's a smoke day in the unit, start by identifying smoke-days per unit
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
    # join in population and smoke PM predictions
    left_join(pop %>% dplyr::select(grid_id_10km = ID, grid_pop_per_m2 = mean)) %>%
    left_join(smokePM) # should still be 119622779 rows


# fill missings with 0s
# calculate pop-weighted avg (density * area) over grid cells in each unit
# (This can take several minutes to run)
avg_county_smokePM <- 
    county_smokePM %>% 
    replace_na(list(smokePM_pred = 0)) %>%
    mutate(area_unclassed = unclass(area), 
           pop = grid_pop_per_m2*area_unclassed) %>%
    group_by(GEOID, date) %>% 
    summarise(smokePM_pred = weighted.mean(smokePM_pred, pop)) %>% 
    ungroup

#note--GEOID is county-specific
saveRDS(avg_county_smokePM, 
        "data/county_smokePM_predictions_20060101_20201231.rds")

#shortcut for once file has been saved
#avg_county_smokePM <- readRDS("data/county_smokePM_predictions_20060101_20201231.rds")

# aggregate to monthly level and create thresholding features
county_smokePM_features <-
    avg_county_smokePM %>% 
    mutate(year = year(date), month = month(date)) %>% 
    group_by(GEOID, year, month) %>% 
    summarise(
        cum_pm2.5 = sum(smokePM_pred),
        mean_pm2.5 =
            case_when(
                month %in% c(1, 3, 5, 7, 8, 10, 12) ~ cum_pm2.5 / 31,
                month %in% c(4, 6, 9, 11) ~ cum_pm2.5 / 30,
                month %in% c(2) ~ cum_pm2.5 / 28
            ),
        daysover0 = sum(smokePM_pred > 0),
        daysover5 = sum(smokePM_pred > 5),
        daysover12.5 = sum(smokePM_pred > 12.5),
        daysover20 = sum(smokePM_pred > 20),
        daysover40 = sum(smokePM_pred > 40)
    ) %>% 
    distinct()

saveRDS(county_smokePM_features, 
        "data/county_smokePM_features_2006_2020.rds")



