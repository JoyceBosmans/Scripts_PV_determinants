pacman::p_load(readxl, writexl, ggplot2,spData, sp, sf, countrycode)

df_PA      <- read.csv(file='df_PA_global.csv')

#~ wrld_simpl <- st_read('/vol/milkunarc/jbosmans/wrld_simpl/TM_WORLD_BORDERS-0.2.shp')

#~ st_crs(wrld_simpl) <- st_crs(4326)

df_PA$lat_orig <- df_PA$y											#keep original lats and lons for plotting (overwritten by st_as_sf)
df_PA$lon_orig <- df_PA$x
df_PA_sf       <- st_as_sf(df_PA, coords = c("x", "y"), crs = 4326)
#~ df_country     <- st_join(df_PA_sf,wrld_simpl, join = st_intersects)
#~ df_country     <- st_set_geometry(df_country, NULL)

#~ #remove non-necessary wrld-simple columns

#~ #check NAs in column NAME:

#~ df_country_xy   <- subset(df_country,select=c('lat_orig','lon_orig'))	
#~ df_nocountry    <- subset(df_country,is.na(NAME))
#~ df_nocountry_xy <- subset(df_nocountry,select=c('lat_orig','lon_orig'))
#~ ggplot() + theme_minimal() + 
#~ geom_point(data=df_country_xy,aes(x=lon_orig,y=lat_orig),size=.5) +  geom_point(data=df_nocountry_xy, aes(x=lon_orig, y=lat_orig),color='red',size=.75)

#~ write.csv(df_country,'df_PA_global_country.csv')

### also try for FMI EEZ database 
EEZ <- st_read('/vol/milkunarc/jbosmans/countries_EEZ/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp')
df_countryEEZ   <- st_join(df_PA_sf,EEZ, join = st_intersects)
df_noEEZ        <- subset(df_countryEEZ,is.na(SOVEREIGN1))
df_noEEZ_xy     <- subset(df_noEEZ,select=c('lat_orig','lon_orig'))
# ==> no missing values! (nrow(df_noEEZ)) is 0)

df_country <- subset(df_countryEEZ,select=c("X","PA","PA_bool","ID", "road_dist", "urban_dist", "grid", "access", "slope", "elev", "protect", "rsds", "wind", "Agriculture", "Forest", "Short_natural", "Wetland", "Urban", "Bare", "Water", "available_area", "lat_orig", "lon_orig", "SOVEREIGN1", "POL_TYPE", "Y_1","x_1","AREA_KM2"))

df_country$continent <- countrycode(sourcevar=df_country$SOVEREIGN1, origin="country.name",destination="continent")
sum(is.na(df_country$continent))	# check that there are no NAs in continent

write_xlsx(df_country,'df_PA_global_country.xlsx')

# column entries in df_country are fine, but if I open the csv elsewhere, column entries seem to have shifted. for instance the values of column 'rsds' end up in column 'protect'.

#~ keep_cols <- c("X","PA","PA_bool","ID", "road_dist", "urban_dist", "grid", "access", "slope", "elev", "protect", "rsds", "wind", "Agriculture", "Forest", "Short_natural", "Wetland", "Urban", "Bare", "Water", "available_area", "lat_orig", "lon_orig", "SOVEREIGN1", "POL_TYPE", "Y_1","x_1","AREA_KM2")
#~ df_countryEEZ[keep_cols]

#~ drop_cols <- c("UNION", "MRGID_EEZ", "TERRITORY1", "MRGID_TER1", "ISO_TER1", "UN_TER1", "MRGID_SOV1", "ISO_SOV1", "UN_SOV1", "TERRITORY2", "MRGID_TER2", "ISO_TER2", "UN_TER2", "SOVEREIGN2", "MRGID_SOV2", "ISO_SOV2", "UN_SOV2",         "TERRITORY3",  "MRGID_TER3", "ISO_TER3", "UN_TER3", "SOVEREIGN3", "MRGID_SOV3", "ISO_SOV3", "UN_SOV3", "geometry" )
#~ df_countryEEZ[, !(names(df_countryEEZ) %in% drop_cols)]
