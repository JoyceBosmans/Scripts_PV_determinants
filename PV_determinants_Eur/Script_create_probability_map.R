# use raster inputs and model from Script_glmer.R
pacman::p_load(ggplot2, spData, sp, sf, raster,writexl,data.table)

# get predictor stack and add grid_km
#~ pred_stack     <- stack('predictor_stack_Eur_261121.gri')
#~ polygon_region <- st_read('/vol/milkunB/jbosmans/SolarWindPotentials_Eur/polygon_europe.shp')	
#~ grid_km_upd    <- raster('/vol/milkunB/jbosmans/PV_determinants_global/grid_km.gri')
#~ grid_km_upd    <- mask(grid_km_upd,polygon_region)	
#~ grid_km_upd    <- crop(grid_km_upd,polygon_region)

#~ pred_stack     <- stack(pred_stack,grid_km_upd)

outputfilename <- 'df_Eur_allgridcells.csv'
outputfilename_raster           <- 'Probability_map_Eur_.gri'
outputfilename_raster_nocountry <- 'Probability_map_Eur_fixed.gri'

load('avg_model.rda')
print('load df instead of redoing stack -> df!. Scale!')
df_country <- read.csv(outputfilename)

# update name of grid_km (forgot when adding this to pred-stack above)
names(df_country)[names(df_country) == 'layer'] <- 'grid_km'

print(colnames(df_country))
print(nrow(df_country))
#~ print('creating dataframe')
#~ df_all    <- as.data.frame(pred_stack,xy=TRUE,na.rm=TRUE)	
#~ sapply(df_all, function(y) sum(length(which(is.na(y)))))

#~ print('write df_all with dataframe for all grid cells - to be overwritten by df_country')
#~ fwrite(df_all,outputfilename)

#~ # add country names ------------------------------------------------------------------
#~ print('dataframe created, add country names')
#~ country_data    <- st_read('/vol/milkunB/jbosmans/SolarWindPotentials_Eur/Europe_data.gpkg')		#created in Script_create_country_polygons.R
#~ EEZ <- st_read('/vol/milkunarc/jbosmans/countries_EEZ/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp')
#~ st_crs(country_data) <- st_crs(4326)			#set to correct coordinate system
#~ df_all$lat_orig <- df_all$y						#keep original lats and lons for plotting (overwritten by st_as_sf)
#~ df_all$lon_orig <- df_all$x
#~ df_all_sf       <- st_as_sf(df_all, coords = c("x", "y"), crs = 4326 )
#~ #df_country      <- st_join(df_all_sf,country_data, join = st_intersects)	#throws an error so use EEZ
#~ df_country      <- st_join(df_all_sf,EEZ, join = st_intersects)
#~ df_country      <- st_set_geometry(df_country, NULL)

# country names from EEZ same as gpkg? (should only be mex - US - CA) has a few others (111 guatemala, 38 cuba, 62 belize, out of 23310688 grid points. using predict with allow.new.levels=TRUE makes sure predict deals with new levels (country names)
#~ df_country$SOVEREIGN1 <- df_country$NAME		#no longer needed if taking country names directly from EEZ

#~ print('write csv with dataframe for all grid cells')
#~ fwrite(df_country,outputfilename)

#~ stop('output df created for all grid cells!')

df_country$road_log   <- log10(df_country$road_dist + 1)	# shift away from 0 to avoid -inf in log-transformed values
df_country$travel_log <- log10(df_country$access + 1)
df_country$grid_log   <- log10(df_country$grid_km + 1)
df_country$slope_log  <- log10(df_country$slope)
df_country$elev_log   <- log10(df_country$elev + abs(min(df_country$elev))+1)

# country name (sovereign1) not included. so working with TERRITORY1
df_country$SOVEREIGN1 <- df_country$TERRITORY1
df_country$SOVEREIGN1[df_country$SOVEREIGN1 %in% c('Azores','Madeira')] <- 'Portugal'
df_country$SOVEREIGN1[df_country$SOVEREIGN1 %in% c('Canary Islands','Ceuta','Melilla')] <- 'Spain'
df_country$SOVEREIGN1[df_country$SOVEREIGN1 %in% c('Guernsey','Jersey')] <- 'United Kingdom'
df_country$SOVEREIGN1[df_country$SOVEREIGN1 %in% c('Jan Mayen','Svalbard')] <- 'Norway'

df_scaled <- df_country
df_scaled[,c('road_log', 'grid_log','travel_log', 'slope_log', 'elev_log', 'rsds', 'Forest', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water', 'available_area')] <- scale(df_country[,c('road_log','grid_log','travel_log', 'slope_log', 'elev_log', 'rsds', 'Forest', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water', 'available_area')],center=TRUE,scale=TRUE)

model_prediction <- predict(avg_model, newdata = df_scaled, type = "response",allow.new.levels=TRUE)

print(paste('Mean model prediction:',round(mean(model_prediction),4),'min:',round(min(model_prediction),4),'max:',round(max(model_prediction),4)))

df_scaled$prediction <- model_prediction
sapply(df_scaled, function(y) sum(length(which(is.na(y)))))

df_for_raster     <- subset(df_scaled,select=c('lon_orig','lat_orig','prediction'))
prediction_raster <- rasterFromXYZ(df_for_raster)


print('save dataframe df_country and raster for Script_compare_to_IMAGE.R')
writeRaster(prediction_raster,filename=outputfilename_raster,overwrite=TRUE,format='raster')

pdf(file='Plot_probability_map_best.pdf',width=10,height=8)
plot(prediction_raster)
dev.off()
    
# create a plot without country-specific intercept (following email Aafke Dec 2)
# check fixef, ranef?
model_prediction <- predict(avg_model, newdata = df_scaled,type = "response",allow.new.levels=TRUE,re.form=NA)

print(paste('Mean model prediction:',round(mean(model_prediction),4),'min:',round(min(model_prediction),4),'max:',round(max(model_prediction),4)))

df_scaled$prediction <- model_prediction
sapply(df_scaled, function(y) sum(length(which(is.na(y)))))

df_for_raster     <- subset(df_scaled,select=c('lon_orig','lat_orig','prediction'))
prediction_raster <- rasterFromXYZ(df_for_raster)

print('save dataframe df_country and raster (without country effect) for Script_compare_to_IMAGE.R')
writeRaster(prediction_raster,filename=outputfilename_raster_nocountry,overwrite=TRUE,format='raster')

prediction_raster <- crop(prediction_raster,extent(-180,-50,10,90))
pdf(file='Plot_probability_map_best_nocountry.pdf',width=10,height=8)
plot(prediction_raster)
dev.off()
