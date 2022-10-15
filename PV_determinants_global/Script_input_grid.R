pacman::p_load(raster)

PowerSystem    = '/vol/milkundata/PowerSystem/'
outputfilename = 'grid_km.tif'
print(outputfilename)

print('Getting distance to grid in m instead of degree')
print('NOTE: an approximation as poleward, 1 deg east-west gets smaller than north-south!!')

# read grid_proximity file (see *PowerSystem/readme)
grid_tif  <- raster(file.path(PowerSystem,'grid_proximity.tif'))

# get distance (length) of sides of grid cells from square root of area in km
grid_dist <- sqrt(area(grid_tif))

# go from degree to meter using grid_proximity (in degree), grid_dist and resolution of 0.01111
distance  <- grid_dist * grid_tif / 0.01111111

writeRaster(distance,filename=outputfilename,overwrite=TRUE,format='raster')


## trying out other stuff
#~ grid_gpkg  <- st_read(file.path(PowerSystem,'grid.gpkg'))
#~ https://gis.stackexchange.com/questions/233443/finding-distance-between-raster-pixels-and-line-features-in-r
#~ test <- SpatialLines(grid_gpkg$geom) # ERROR ; lines list not exclusively filled with Lines objects
