pacman::p_load(raster)

dirMirza  = '/vol/milkunarc/mcengic/Projects/Agriculture_modeling/Data/Predictors_final/'

# split world into 60x60 degree boxes, then aggregate to 0.011 resolution, merge and check 

slope <- raster(file.path(dirMirza,'Slope_fnl.tif'))

extent_A <- extent(-180,-120,30,90)
extent_B <- extent(-120,-60,30,90)
slope_A  <- crop(slope,extent_A)
slope_B  <- crop(slope,extent_B)

slope_A_mean <- aggregate(slope_A,fact=4,fun=mean)
slope_B_mean <- aggregate(slope_B,fact=4,fun=mean)

slope_merged <- merge(slope_A_mean,slope_B_mean)

# zoom in to the 'boundary' at -120deg and check that there are not missing values
extent_check <- extent(-120.2,-119.8,45,46)
slope_check <- crop(slope_merged,extent_check)
plot(slope_check)
