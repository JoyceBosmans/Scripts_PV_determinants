pacman::p_load(raster)
args = commandArgs(trailingOnly=TRUE)
# args should be: area-letter lon-left lon-right lat-low lat-up

PowerSystem    = '/vol/milkundata/PowerSystem/'
outputfilename = paste0('predictor_stack_',args[1],'.gri')
print(outputfilename)

print('Getting distance to grid in m instead of degree')

# split world into 60x60 degree boxes, then aggregate to 0.011 resolution
extent_region <- extent(as.numeric(args[2]),as.numeric(args[3]),as.numeric(args[4]),as.numeric(args[5]))

#~ urban_dist <- aggregate(urban_dist,fact=4,fun=mean)

grid_tif  <- raster(file.path(PowerSystem,'grid.tif'))
grid_tif  <- crop(grid_tif,extent_region)
grid_tif[grid_tif < 255] <- NA	# leave in only cells with grids (set to 255) for distance computation
distance  <- distance(grid_tif)

writeRaster(distance,filename=outputfilename,overwrite=TRUE,format='raster')


## trying out other stuff
#~ grid_gpkg  <- st_read(file.path(PowerSystem,'grid.gpkg'))
#~ https://gis.stackexchange.com/questions/233443/finding-distance-between-raster-pixels-and-line-features-in-r
#~ test <- SpatialLines(grid_gpkg$geom) # ERROR ; lines list not exclusively filled with Lines objects
