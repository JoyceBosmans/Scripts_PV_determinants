pacman::p_load(ggplot2, spData, sp, sf, raster,writexl,data.table,INLA,unix,plyr)

### set output file names etc
df_Asia_allgridcells			<- 'df_Asia_allgridcells.csv'
outputfilename_raster           <- 'Probability_map_Asia.gri'
outputfilename_raster_nocountry <- 'Probability_map_Asia_fixed.gri'
best_model                      <- '1138'
dir_Konrad 						<- '/vol/milkunarc2/kmielke/pv' 
load(file = file.path(dir_Konrad,'/data/rda/df_PA_Asia_country_060622_proprocessed.Rda'))
EEZ <- st_read('/vol/milkunarc/jbosmans/countries_EEZ/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp')

### if updated file already present in this dir, load and go ahead. Otherwise add updated grid_km, store, then continue.
if(file.exists(df_Asia_allgridcells)) {
  print('loading dataframe with all grid cells')
  df_full <- read.csv(df_Asia_allgridcells)
} else {
  print('updating dataframe with grid_km and storing in this dir')
  ### dataframe with all grid cells already created in ../PV_determinants_Asia/Script_create_probability_maps.R
  df_full <- read.csv(file.path('../PV_determinants_Asia/df_Asia_allgridcells.csv'))
  print('done loading. df_full now contains:')
  print(colnames(df_full))
  print(Sys.time())
  
  ### add updated grid distance computations
  grid_km_upd        <- raster('/vol/milkundata/PowerSystem/final_distance.tif')
  names(grid_km_upd) <- c('grid_km')
  sel_lonlat  	     <- subset(df_full,select=c('lon_orig','lat_orig'))
  values_grid_km 	 <- raster::extract(grid_km_upd,sel_lonlat,df=TRUE)
  df_full$grid_orig  <- df_full$grid_km
  df_full$grid_km    <- values_grid_km$grid_km 
  # check!
  
  print('done adding grid_km . df_full now contains:')
  
  print(colnames(df_full))
  print(Sys.time())
  # somehow for Asia country names (SOVEREIGN1) are missing so add these
  df_full$y          <- df_full$lat_orig											
  df_full$x          <- df_full$lon_orig
  df_full_sf         <- st_as_sf(df_full, coords = c("x", "y"), crs = 4326)

  df_full            <- st_join(df_full_sf,EEZ, join = st_intersects)

  print('done adding SOVEREIGN1. df_full now contains:')
  print(colnames(df_full))
  print(Sys.time())
  
  df_full            <- subset(df_full,select=c('grid','protect','access','rsds','road_dist','slope','elev','Agriculture','Forest','Short_natural','Wetland','Urban','Bare','Water','available_area','lat_orig','lon_orig','SOVEREIGN1','grid_km', 'grid_orig','Y_1','x_1'))

  # store csv in this dir rather than redoing grid distance addition. Notice a small number of points don't have grid_km values (islands / coasts, 57 out of 12859852)
  df_full <- df_full[complete.cases(df_full$grid_km),]	
  fwrite(df_full,df_Asia_allgridcells)
}

