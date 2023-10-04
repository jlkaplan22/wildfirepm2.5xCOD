#### Load and clean PRISM data with population weighting ####
# Process is similar to `01_county_pm2.5.R`

# download PRISM temp data
options(prism.path=paste0(raw_data_dir, "prism/temp"))
get_prism_monthlys(type="tmean", years = 2006:2020, mon = seq(1:12), keepZip=FALSE)
temp_stack <- pd_stack(prism_archive_ls())

options(prism.path=paste0(raw_data_dir, "prism/precip"))
get_prism_monthlys(type="ppt", years = 2006:2020, mon = seq(1:12), keepZip=FALSE)
precip_stack <- pd_stack(prism_archive_ls())

# load county sf
counties_sf <- 
    counties %>% 
    filter(STATEFP %in% non_CONUS_FIPS == F) %>% 
    dplyr::select(GEOID)

# read in the grid
grid_10km <- 
    st_read(paste0(raw_data_dir, "10km_grid/10km_grid_wgs84/10km_grid_wgs84.shp")) %>% 
    st_transform(st_crs(counties_sf))

# extract PRISM metrics
temp_extract_10km <- 
    cbind(
        grid_10km$ID,
        exact_extract(terra::rast(temp_stack), grid_10km, fun="mean", progress=T)
    ) %>% 
    rename(ID = `grid_10km$ID`)

precip_extract_10km <- 
    cbind(
        grid_10km$ID,
        exact_extract(terra::rast(precip_stack), grid_10km, fun="mean", progress=T)
    ) %>% 
    rename(ID = `grid_10km$ID`)

# merge in population data
pop <-
    list.files("data_raw/populationDensity_10km_subgrid", full.names = TRUE) %>% 
    purrr::map_dfr(read_csv)

# load grid-to-county xwalk from `01_county_pm2.5.R`
county_cross <- readRDS("data/county_xwalk")

# tidy data
tidy_temp_10km <-
    temp_extract_10km %>% 
    st_drop_geometry() %>% 
    pivot_longer(cols = -c(ID), names_to = "date", values_to = "temp") %>% 
    mutate(
        year = str_sub(date, 31, 34),
        month = str_sub(date, 35, 36)
    ) %>% 
    dplyr::select(-date)

tidy_precip_10km <-
    precip_extract_10km %>% 
    st_drop_geometry() %>% 
    pivot_longer(cols = -c(ID), names_to = "date", values_to = "precip") %>% 
    mutate(
        year = str_sub(date, 29, 32),
        month = str_sub(date, 33, 34)
    ) %>% 
    dplyr::select(-date)

temp_county <-
    tidy_temp_10km %>% 
    left_join(
        pop %>% dplyr::select(ID, grid_pop_per_m2 = mean),
        by=c("ID")
    ) %>% 
    left_join(
        county_cross,
        by=c("ID" = "grid_id_10km")
    ) %>% 
    mutate(area2 = unclass(area), 
           pop = grid_pop_per_m2*area2) %>%
    group_by(GEOID, year, month) %>% 
    summarise(weighted_temp = weighted.mean(temp, pop)) %>% 
    ungroup()

precip_county <-
    tidy_precip_10km %>% 
    left_join(
        pop %>% dplyr::select(ID, grid_pop_per_m2 = mean),
        by=c("ID")
    ) %>% 
    left_join(
        county_cross,
        by=c("ID" = "grid_id_10km")
    ) %>% 
    mutate(area2 = unclass(area), 
           pop = grid_pop_per_m2*area2) %>%
    group_by(GEOID, year, month) %>% 
    summarise(weighted_precip = weighted.mean(precip, pop)) %>% 
    ungroup()

    
# final PRISM data
PRISM_popw <-
    full_join(
        temp_county,
        precip_county
    ) %>% 
    dplyr::select(GEOID, year, month, weighted_temp, weighted_precip) %>% 
    mutate(month = as.numeric(month), year = as.numeric(year))

write_csv(PRISM_popw, paste0(data_dir, "prism_monthly_popw.csv"))

# shortcut for once this file has been run initially
#PRISM_popw <- read_csv(paste0(data_dir, "prism_monthly_popw.csv"))

#compare results to area-weighted results, should be mostly similar but not exactly colinear
compare <-
    full_join(
        PRISM_popw,
        PRISM
    )

compare %>% 
    ggplot(aes(x=temp, y=weighted_temp)) +
    geom_point(alpha=.1) +
    geom_abline(slope = 1, linewidth = .1) 

compare %>% 
    ggplot(aes(x=precip, y=weighted_precip)) +
    geom_point(alpha=.1) +
    geom_abline(slope = 1, linewidth = .1) 

