pacman::p_load(rgdal, readxl, writexl, ggplot2, spData, sf, sp, raster,dplyr,mapview)

# currently using wrld-simpl, see https://cran.r-project.org/web/packages/maptools/maptools.pdf and https://github.com/nasa/World-Wind-Java/tree/master/WorldWind/testData/shapefiles
# data in /vol/milkunarc/jbosmans/wrld_simpl

wrld_simpl <- st_read('/vol/milkunarc/jbosmans/wrld_simpl/TM_WORLD_BORDERS-0.2.shp')

# see SolarWindPotentials/IMAGE_regions.txt and https://models.pbl.nl/image/index.php/Region_classification_map
# check country names against wrld_simpl$NAME

region		   <- c('Australia','New Zealand','American Samoa', 'Cook Islands', 'Fiji', 'French Polynesia', 'Kiribati', 'Marshall Islands', 'Micronesia, Federated States of',  'New Caledonia', 'Niue', 'Northern Mariana Islands', 'Palau', 'Pitcairn Islands', 'Samoa', 'Solomon Islands', 'Tokelau', 'Tonga', 'Tuvalu', 'Vanuatu', 'Wallis and Futuna Islands')

first_to_add = TRUE
for(country in region) {
	print(country)
	country_polygon <- assign(country,wrld_simpl[wrld_simpl$NAME == country, 0])
	if(dim(country_polygon)[1] == 1 & first_to_add){
		first_to_add = FALSE
		polygon_region <- country_polygon
	} else if(dim(country_polygon)[1] == 1) {
		polygon_region <- st_union(polygon_region,country_polygon)
	}
}

Asia_data <- wrld_simpl %>% filter(NAME %in%  region) 

st_write(polygon_region,"polygon_Oceania.shp",driver="ESRI shapefile",update=TRUE)
st_write(Asia_data,'Oceania_data.gpkg',append=TRUE)
