pacman::p_load(raster)
args = commandArgs(trailingOnly=TRUE)
# args should be: area-letter lon-left lon-right lat-low lat-up

dirMirza       = '/vol/milkunarc/mcengic/Projects/Agriculture_modeling/Data/Predictors_final/'
rsds_file      = '/vol/milkundata/ERA5/irradiation_average/rsds_kwh_av_1988_2017.nc'
wind_file      = '/vol/milkundata/GlobalWindAtlas/gwa3_250_wind-speed_50m.tif'
outputfilename = paste0('predictor_stack_',args[1],'.gri')
print(outputfilename)

print('CHECK whether script is activated for all variables or a subset')

# split world into 60x60 degree boxes, then aggregate to 0.011 resolution
extent_region <- extent(as.numeric(args[2]),as.numeric(args[3]),as.numeric(args[4]),as.numeric(args[5]))

#~ road_dist <- raster(file.path(dirMirza,'ESA_road_distance_fnl.tif'))	    # actually from GRIP
#~ road_dist <- crop(road_dist,extent_region)
#~ road_dist <- aggregate(road_dist,fact=4,fun=mean)

#~ urban_dist <- raster(file.path(dirMirza,'ESA_urban_distance_fit_fnl.tif'))	# fit: 2003-2013 (represents 2002)
#~ urban_dist <- crop(urban_dist,extent_region)								# ESA Urban: from Global Human Settlement Layer and Global Urban Footprint (ghsl.jrc.ec.europa.eu, GUF dlr.de)
#~ urban_dist <- aggregate(urban_dist,fact=4,fun=mean)

slope  <- raster(file.path(dirMirza,'Slope_fnl.tif'))
slope  <- crop(slope,extent_region)
slope  <- aggregate(slope,fact=4,fun=mean)

#~ grid <- raster('/vol/milkundata/PowerSystem/grid_proximity.tif')
#~ grid <- crop(grid,extent_region)

#~ print('Slope and grid done')

#~ access <- raster('/vol/milkundata/TravelTimes/travel_time_to_cities_12.tif')
#~ access[access == 65535] <- NA
#~ access <- crop(access,extent_region)
#~ access <- resample(access,slope,method='bilinear')

#~ print('access done, get elevation')
#~ elev <- raster(file.path(dirMirza,'DEM_fnl.tif'))
#~ elev <- crop(elev,extent_region)
#~ elev <- aggregate(elev,fact=4,fun=mean)

#~ print('get protected status')
#~ protect <- raster(file.path(dirMirza,'Protected_areas_fnl.tif'))
#~ protect <- crop(protect,extent_region)
#~ protect <- aggregate(protect,fact=4,fun=modal)	

print('get rsds')
rsds <- raster(rsds_file,varname='ssrd')
rsds <- rotate(rsds)						#lon rsds 0:360 to -180:180
rsds <- crop(rsds,extent_region)	
rsds <- resample(rsds,slope,method='ngb')	#no bilinear interpretation

#~ print('get wind')
#~ wind <- raster(wind_file)	
#~ wind <- crop(wind,extent_region)
#~ wind <- aggregate(wind,fact=4,fun=mean)
#~ wind <- resample(wind,slope,method='bilinear')

#~ # store this (as a raster stack)
#~ pred_stack <- stack(road_dist,urban_dist,grid,access,slope,elev,protect,rsds,wind)
#~ writeRaster(pred_stack,filename=outputfilename,overwrite=TRUE,format='raster')

writeRaster(rsds,filename=outputfilename,overwrite=TRUE,format='raster')

print('NOTE: add other parameters (check which) and stack those before saving. ')
print('      no land-sea masking done yet (do later when selecting absences, based on e.g. slope?)')
# see Script_input_western_europe_5nov.R
# mind memory (currently ~16GB left)
