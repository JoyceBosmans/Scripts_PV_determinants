pacman::p_load(raster,rgdal)

### updating predictors road distance, slope and elevation. 
### later add to other predictors already created in SolarWindPotentials_Global/Script_input_global.R and Script_input_global_split.R

# road distance in km to nearest roads, 10 arcsec, aggregate from 0.002777 to 0.011 (~1km)
GRIP      <- raster('/vol/milkunarc/jbosmans/GRIP/GRIP4_distance_km_tp12345_10sec.tif')
road_dist <- aggregate(GRIP,fact=4,fun=mean)
#~ writeRaster(road_dist,'road_dist_251121.tif',format='raster')

# elevation at 3 arcsec. Aggregate to 0.01 then resample to 0.11
merit <- raster('/vol/milkundata/Merit_DEM/Merit_DEM_mosaic.tif')
merit <- aggregate(merit,fact=12,fun=mean)    
#~ writeRaster(merit,'merit_01_251121.tif',format='raster')

merit <- resample(merit,road_dist,method='bilinear')
#~ writeRaster(merit,'merit_DEM_251121.tif',format='raster')

# create the slope outside of R (due to errors with rgdal), using merit*tif:
# use scale=11140 to account for the ratio degree (horizontal) to meters (vertical)
# gdaldem slope merit_DEM_251121.tif merit_slope_251121.tif -s 111140

# store road_dist, elev and slope in 1 file (after loading slope, mind variable names): 
pred_stack <- stack(road_dist,slope,merit)
writeRaster(pred_stack,filename='predictor_stack_global_251121.gri',format='raster')
