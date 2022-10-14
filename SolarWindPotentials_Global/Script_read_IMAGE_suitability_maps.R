pacman::p_load(raster)

dirIMAGE = '/vol/milkunB/jbosmans/IMAGE_suitability'

PV   <- raster(file.path(dirIMAGE,'CSP_PV_Suitability_map.asc'))
wind <- raster(file.path(dirIMAGE,'Onshore_offshore_wind_Suitability_map.asc'))

