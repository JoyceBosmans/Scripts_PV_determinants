pacman::p_load(ggplot2, spData, sp, sf, raster,writexl,data.table,readxl,INLA)

### set output file names, dirs etc
df_Afr_allgridcells				<- 'df_Afr_allgridcells.csv'
outputfilename_raster           <- 'Probability_map_Afr.gri'
#outputfilename_raster_nocountry <- 'Probability_map_Afr_fixed.gri'
#best_model						<- '3373'
dir_Konrad 						<- '/vol/milkunarc2/kmielke/pv' 
#load(file = file.path(dir_Konrad,'/data/rda/df_PA_Afr_country_060622_proprocessed.Rda'))
EEZ <- st_read('/vol/milkunarc/jbosmans/countries_EEZ/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp')

### if updated file already present in this dir, load and go ahead. Otherwise add updated grid_km, store, then continue.
if(file.exists(df_Afr_allgridcells)) {
  print('loading dataframe with all grid cells')
  df_full <- read.csv(df_Afr_allgridcells)
} else {
  print('creating dataframe out of stack')
  pred_stack <- stack('/vol/milkunB/jbosmans/PV_determinants_Afr/predictor_stack_Afr.gri')
  df_all     <- as.data.frame(pred_stack,xy=TRUE,na.rm=TRUE)	
  sapply(df_all, function(y) sum(length(which(is.na(y)))))

  df_all_sf  <- st_as_sf(df_all, coords = c("x", "y"), crs = 4326)

  df_all     <- st_join(df_all_sf,EEZ, join = st_intersects)

  df_all     <- subset(df_all,select=c('grid','protect','access','rsds','road_dist','slope','elev','Agriculture','Forest','Short_natural','Wetland','Urban','Bare','Water','available_area','SOVEREIGN1','grid_km','Y_1','x_1'))
  
  print('write df_all with dataframe for all grid cells')
  fwrite(df_all,df_Afr_allgridcells)
  
  print(paste('number of gridcells:',nrow(df_all)))
  table(df_all$SOVEREIGN1)
}

#~   # somehow for African country names (SOVEREIGN1) are missing so add these
#~   df_all$y          <- df_all$lat_orig											
#~   df_all$x          <- df_all$lon_orig

#~   fwrite(df_all,df_Afr_allgridcells)	# not yet with lambert coordinats. Add those when creating stk.pred
