pacman::p_load(rgdal, readxl, writexl, ggplot2, spData, sf, sp, raster,dplyr,mapview)

outputfilename = 'predictor_stack_global.tif'	
dirMirza  = '/vol/milkunarc/mcengic/Projects/Agriculture_modeling/Data/Predictors_final/'
rsds_file = '/vol/milkundata/ERA5/irradiation_average/rsds_kwh_av_1988_2017.nc'
wind_file = '/vol/milkundata/GlobalWindAtlas/gwa3_250_wind-speed_50m.tif'	#wind at 50m (to do: later on check at other heights?)

# start with slope, elevation or protect, resample road_dist and urban_dist
# resample from 300m to ~1km (fact=4, or 0.0027778 to 0.011 deg)

slope <- raster(file.path(dirMirza,'Slope_fnl.tif'))
slope <- aggregate(slope,fact=4,fun=mean)

elev <- raster(file.path(dirMirza,'DEM_fnl.tif'))
elev <- aggregate(elev,fact=4,fun=mean)

protect <- raster(file.path(dirMirza,'Protected_areas_fnl.tif'))
protect <- aggregate(protect,fact=4,fun=modal)

road_dist <- raster(file.path(dirMirza,'ESA_road_distance_fnl.tif'))
road_dist <- aggregate(road_dist,fact=4,fun=mean)
road_dist <- mask(road_dist,slope)	#delete values over ocean

urban_dist <- raster(file.path(dirMirza,'ESA_urban_distance_fit_fnl.tif'))	# TO DO: check whether I'm using the correct file
urban_dist <- aggregate(urban_dist,fact=4,fun=mean)
urban_dist <- mask(urban_dist,slope) #delete values over ocean

rsds <- raster(rsds_file,varname='ssrd')
rsds <- rotate(rsds)						#lon rsds 0:360 to -180:180
rsds <- resample(rsds,slope,method='ngb')	#no bilinear interpretation
rsds <- mask(rsds,slope)

wind <- raster(wind_file)
wind <- aggregate(wind,fact=4,fun=mean)		#from 0.0025 deg to 0.01 deg
wind <- resample(wind,slope,method='bilinear')	#to 0.011 deg
wind <- mask(wind,slope)
#plot(wind)

# note there are different maps available from Nelson et al. Chose the travel time to all cities rather than that to only big or only small cities. 
print('get accessibility')
access <- raster('/vol/milkundata/TravelTimes/travel_time_to_cities_12.tif')
access <- resample(access,slope,method='bilinear')
access <- mask(access,slope)

grid <- raster('/vol/milkundata/PowerSystem/grid_proximity.tif')

print('Check all predictors (plot) before saving and continuing')
#----------------------------------------------------------


# output as *gri (instead of *tif)
pred_stack <- stack(grid,access,road_dist,urban_dist,slope,elev,protect,rsds,wind)
writeRaster(pred_stack,filename=outputfilename,overwrite=TRUE,format='raster')	#add format to retain layer names

