#### Load and clean PRISM data ####

# download PRISM temp data
temp_stack_06 <- prism_helper("2006", "temp")
temp_stack_07 <- prism_helper("2007", "temp")
temp_stack_08 <- prism_helper("2008", "temp")
temp_stack_09 <- prism_helper("2009", "temp")
temp_stack_10 <- prism_helper("2010", "temp")
temp_stack_11 <- prism_helper("2011", "temp")
temp_stack_12 <- prism_helper("2012", "temp")
temp_stack_13 <- prism_helper("2013", "temp")
temp_stack_14 <- prism_helper("2014", "temp")
temp_stack_15 <- prism_helper("2015", "temp")
temp_stack_16 <- prism_helper("2016", "temp")


# download PRISM precip data
precip_stack_06 <- prism_helper("2006", "precip")
precip_stack_07 <- prism_helper("2007", "precip")
precip_stack_08 <- prism_helper("2008", "precip")
precip_stack_09 <- prism_helper("2009", "precip")
precip_stack_10 <- prism_helper("2010", "precip")
precip_stack_11 <- prism_helper("2011", "precip")
precip_stack_12 <- prism_helper("2012", "precip")
precip_stack_13 <- prism_helper("2013", "precip")
precip_stack_14 <- prism_helper("2014", "precip")
precip_stack_15 <- prism_helper("2015", "precip")
precip_stack_16 <- prism_helper("2016", "precip")

# read in county shapefiles
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

# extract values to 10km2 grid
tic()
temp_extract_06 <- exact_extract(temp_stack_06, grid_10km, fun="mean", progress=T, max_cells_in_memory = 307182540)
toc()

temp_extract_06 %>% filter(is.na(`mean.PRISM_tmean_stable_4kmD2_20061231_bil`)) %>% nrow() #2% of gridcells are NA

#temp_extract_06 <- terra::extract(temp_stack_06, grid_10km, fun=mean) #takes too long, >500s, I stopped it at 517s


