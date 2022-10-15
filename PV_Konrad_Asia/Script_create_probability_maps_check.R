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
  print(paste('nrow of df_full:',nrow(df_full),'should be 47489997'))
  print('latitude range:')
  print(paste(min(df_full$lat_orig),max(df_full$lat_orig)))
  print('longitude range:')
  print(paste(min(df_full$lon_orig),max(df_full$lon_orig)))
} else {
  print('This should not be printed')
}

