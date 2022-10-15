pacman::p_load(rgdal, readxl, writexl, ggplot2, spData, sf, sp, raster,dplyr,mapview)

# currently using wrld-simpl, see https://cran.r-project.org/web/packages/maptools/maptools.pdf and https://github.com/nasa/World-Wind-Java/tree/master/WorldWind/testData/shapefiles
# data in /vol/milkunarc/jbosmans/wrld_simpl

wrld_simpl <- st_read('/vol/milkunarc/jbosmans/wrld_simpl/TM_WORLD_BORDERS-0.2.shp')

# Combining Russia, Central Asia, Middle East, Southeast Asia, Indonesia, India, Rest of South Asia, China, Korea, Japan, and Oceania from IMAGE region classification map
# see SolarWindPotentials/IMAGE_regions.txt and https://models.pbl.nl/image/index.php/Region_classification_map
# check country names against wrld_simpl$NAME

Russia_central 	 <- c('Russia','Kazakhstan','Kyrgyzstan','Tajikistan','Turkmenistan','Uzbekistan','Armenia','Azerbaijan','Georgia')

Middle_southeast <- c('Bahrain','Iran (Islamic Republic of)','Iraq','Israel','Jordan','Kuwait','Lebanon','Oman','Qatar','Saudi Arabia','Syrian Arab Republic','Untied Arab Emirates','Yemen', 'Brunei Darussalam', 'Cambodia', 'Lao People\'s Democratic Republic', 'Malaysia', 'Philippines', 'Singapore', 'Burma','Viet Nam', 'Thailand')

Indo_India_south <- c('Timor-Leste', 'Indonesia', 'Papua New Guinea','India', 'Afghanistan', 'Bangladesh', 'Bhutan', 'Maldives', 'Nepal', 'Pakistan', 'Sri Lanka')

Chin_Kor_Jan_Oc <- c('China','Hong Kong','Macau','Mongolia','Taiwan','Korea, Democratic People\'s Republic of','Japan','Korea, Republic of')

APAC <- c('Russia','Kazakhstan','Kyrgyzstan','Tajikistan','Turkmenistan','Uzbekistan','Armenia','Azerbaijan','Georgia','Bahrain','Iran (Islamic Republic of)','Iraq','Israel','Jordan','Kuwait','Lebanon','Oman','Qatar','Saudi Arabia','Syrian Arab Republic','Untied Arab Emirates','Yemen', 'Brunei Darussalam', 'Cambodia', 'Lao People\'s Democratic Republic', 'Malaysia', 'Philippines', 'Singapore', 'Burma','Viet Nam', 'Thailand','Timor-Leste', 'Indonesia', 'Papua New Guinea','India', 'Afghanistan', 'Bangladesh', 'Bhutan', 'Maldives', 'Nepal', 'Pakistan', 'Sri Lanka','China','Hong Kong','Macau','Mongolia','Taiwan','Korea, Democratic People\'s Republic of','Japan','Korea, Republic of')

region <- Russia_central

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
polygon_Russia_central <- polygon_region

region <- Middle_southeast

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
polygon_Middle_southeast <- polygon_region

region <- Indo_India_south

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
polygon_Indo_India_south <- polygon_region

region <- Chin_Kor_Jan_Oc

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
polygon_Chin_Kor_Jan_Oc <- polygon_region

polygon_region <- st_union(polygon_Russia_central,polygon_Middle_southeast,polygon_Indo_India_south,polygon_Chin_Kor_Jan_Oc,polygon_Indo_India_south,polygon_Chin_Kor_Jan_Oc)

region <- APAC
Asia_data <- wrld_simpl %>% filter(NAME %in%  region) 
Asia_polygon <- Asia_data$geometry
st_write(Asia_polygon,"polygon_Asia.shp",driver="ESRI shapefile",update=TRUE)
st_write(Asia_data,'Asia_data.gpkg',append=TRUE)
