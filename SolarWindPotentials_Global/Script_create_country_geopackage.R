pacman::p_load(rgdal, spData, sf, sp)

# currently using wrld-simpl, see https://cran.r-project.org/web/packages/maptools/maptools.pdf and https://github.com/nasa/World-Wind-Java/tree/master/WorldWind/testData/shapefiles
# data in /vol/milkunarc/jbosmans/wrld_simpl

wrld_simpl <- st_read('/vol/milkunarc/jbosmans/wrld_simpl/TM_WORLD_BORDERS-0.2.shp')

#currently simply storing the shape file as a geopackage for further use
st_write(wrld_simpl,'wrld_simpl_global.gpkg',append=TRUE)

#error messages 
#try to link the original shape file to the P-A dataframe in separate script later on?
