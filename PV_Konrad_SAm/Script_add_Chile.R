pacman::p_load(rgdal, readxl, writexl, ggplot2, spData, sf, sp, raster,dplyr,mapview,data.table)

# somehow Chile was missing from df_full; it was missing from SAm_data.gpkg

pred_stack     <- stack('/vol/milkunB/jbosmans/PV_determinants_SAm/predictor_stack_SAm_261121.gri')
outputfilename <- 'df_Chile_allgridcells.csv'
wrld_simpl     <- st_read('/vol/milkunarc/jbosmans/wrld_simpl/TM_WORLD_BORDERS-0.2.shp')

# currently using wrld-simpl, see https://cran.r-project.org/web/packages/maptools/maptools.pdf and https://github.com/nasa/World-Wind-Java/tree/master/WorldWind/testData/shapefiles

#~ first_to_add = TRUE

region <- c('Chile')
for(country in region) {
	country_polygon <- assign(country,wrld_simpl[wrld_simpl$NAME == country, 0])
	if(dim(country_polygon)[1] == 1 & first_to_add){
		first_to_add = FALSE
		polygon_region <- country_polygon
	} else if(dim(country_polygon)[1] == 1) {
		polygon_region <- st_union(polygon_region,country_polygon)
	}
}
plot(polygon_region)

pred_stack_reg    <- mask(pred_stack,polygon_region)	

df_all            <- as.data.frame(pred_stack_reg,xy=TRUE,na.rm=TRUE)
df_all$SOVEREIGN1 <- 'Chile' 	# manually because st_join gives error

#~ Chile_data <- wrld_simpl %>% filter(NAME %in%  'Chile') 
#~ df_all    <- as.data.frame(pred_stack,xy=TRUE,na.rm=TRUE)	

#~ st_crs(Chile_data) <- st_crs(4326)			#set to correct coordinate system
#~ df_all$lat_orig <- df_all$y						#keep original lats and lons for plotting (overwritten by st_as_sf)
#~ df_all$lon_orig <- df_all$x
#~ df_all_sf       <- st_as_sf(df_all, coords = c("x", "y"), crs = 4326 )
#~ df_country      <- st_join(df_all_sf,Chile_data, join = st_intersects)
#~ df_country      <- st_set_geometry(df_country, NULL)

print(unique(df_all$SOVEREIGN1))

print('remember to add Chile and rerun addition of grid distnace in Create*map script')

fwrite(df_all,outputfilename)
