pacman::p_load(readxl, writexl, ggplot2, raster,dismo,sf,countrycode)

print('This script is used to create the predictor stack. Dataframe with P and A was created in Script_PA_predictors_add.R')
print('Predictor stack is still needed, for creating the final probability map (Script_create_probability_map.R)')

### reading and preparing input
### ----------------------------------------------------------------------------------------------------
polygon_region <- st_read('/vol/milkunB/jbosmans/SolarWindPotentials_NAm/polygon_NAm.shp')		# change for other regions!
region         <- c('United States', 'Canada', 'Mexico')	    # change for other regions!

pred_stack     <- stack('/vol/milkunB/jbosmans/SolarWindPotentials_Global/pred_stack_global.gri')
ESA_stack      <- stack('/vol/milkunB/jbosmans/SolarWindPotentials_Global/ESA_stack_2000.gri')
pred_stack_upd <- stack('/vol/milkunB/jbosmans/PV_determinants_global/predictor_stack_global_251121.gri')	# road_dist, slope, elev
names(ESA_stack)      <- c('Agriculture','Forest','Short_natural','Wetland','Urban','Bare','Water','available_area')
names(pred_stack_upd) <- c('road_dist','slope','elev')

# merge pred_stack (without road_dist, urban_dist, slope, elev) and pred_stack_upd
print('creating new pred_stack')
pred_stack  <- subset(pred_stack,c('grid','protect','access','rsds'))
pred_stack  <- extend(pred_stack,pred_stack_upd)	#extend to -90:90

# add grid in kilometers (so note, 'grid' is in degrees, 'grid_km' in km)
grid_km_upd        <- raster('/vol/milkunB/jbosmans/PV_determinants_global/grid_km.gri')
grid_km_upd        <- extend(grid_km_upd,pred_stack_upd)	#extend to -90:90
names(grid_km_upd) <- c('grid_km')

pred_stack  <- stack(pred_stack,pred_stack_upd,grid_km_upd)

print('creating regional pred_stack')
pred_stack_reg <- mask(pred_stack,polygon_region)					# this masking / cropping is time consuming (~hour)
pred_stack_reg <- crop(pred_stack_reg,polygon_region)

#~ print('creating regional ESA stack')
ESA_stack_reg  <- mask(ESA_stack,polygon_region)	
ESA_stack_reg  <- crop(ESA_stack_reg,polygon_region)

predictors     <- stack(pred_stack_reg,ESA_stack_reg)

#~ # TO DO: temporarily store input stacks, for debugging
outputfilename_stack <- 'predictor_stack_NAm_261121.gri'
writeRaster(predictors,filename=outputfilename_stack,overwrite=TRUE,format='raster')

#~ print('stop script for now after creating outputfile raster')
#~ stop()

