pacman::p_load(readxl, writexl, ggplot2, raster,dismo,sf,countrycode)

### reading and preparing input
### ----------------------------------------------------------------------------------------------------
polygon_region <- st_read('/vol/milkunB/jbosmans/SolarWindPotentials_Afr/polygon_Afr.shp')			# change for other regions!
outputfilename <- 'df_PA_Afr_country.xlsx'						# change for other regions!
region         <- c('Algeria', 'Egypt', 'Libya', 'Morocco', 'Tunisia', 'Western Sahara','Benin', 'Burkina Faso', 'Cameroon', 'Cape Verde', 'Central African Republic', 'Chad', 'Democratic Republic of the Congo', 'Congo', 'Ivory Coast', 'Equatorial Guinea', 'Gabon', 'Gambia', 'Ghana', 'Guinea', 'Guinea-Bissau', 'Liberia', 'Mali', 'Mauritania', 'Niger', 'Nigeria', 'Sao Tome and Principe', 'Senegal', 'Sierra Leone', 'Saint Helena', 'Togo','Burundi', 'Comoros', 'Djibouti', 'Eritrea', 'Ethiopia', 'Kenya', 'Madagascar', 'Mauritius', 'Réunion', 'Rwanda', 'Seychelles', 'Somalia', 'Sudan', 'Uganda', 'South Africa', 'Angola', 'Botswana', 'Lesotho', 'Malawi', 'Mozambique', 'Namibia', 'Swaziland', 'Tanzania', 'Zambia', 'Zimbabwe')

wiki_df        <- read_excel('/vol/milkunB/jbosmans/WikiSolar/200929_RUN365_B99--.xlsx')
wiki_df        <- wiki_df[wiki_df$Country %in% region,]
wiki_df        <- wiki_df[complete.cases(wiki_df$"Lat,Long"),]    			 # delete locations without location
wiki_df        <- tidyr::separate(data = wiki_df, col = "Lat,Long", into = c("lat_wiki", "lon_wiki"), sep = "\\,")
wiki_df        <- transform(wiki_df,lon_wiki=as.numeric(lon_wiki))
wiki_df        <- transform(wiki_df,lat_wiki=as.numeric(lat_wiki))

print(paste('Number of PV facilities with lat,lon in this region:',nrow(wiki_df)))
wiki_df        <- wiki_df[wiki_df$Status %in% c('A','B'),]					# keep only operating or near-completed facilities
print(paste('   of which',nrow(wiki_df),'are currently operating'))
wiki_df        <- subset(wiki_df, !Pt_TcArr== 'Floating' | is.na(Pt_TcArr))	# delete floating PV
print(paste('   of which',nrow(wiki_df),'are non-floating'))

#~ pred_stack     <- stack('/vol/milkunB/jbosmans/SolarWindPotentials_Global/pred_stack_global.gri')
#~ ESA_stack      <- stack('/vol/milkunB/jbosmans/SolarWindPotentials_Global/ESA_stack_2000.gri')
#~ pred_stack_upd <- stack('/vol/milkunB/jbosmans/PV_determinants_global/predictor_stack_global_251121.gri')	# road_dist, slope, elev
#~ names(ESA_stack)      <- c('Agriculture','Forest','Short_natural','Wetland','Urban','Bare','Water','available_area')
#~ names(pred_stack_upd) <- c('road_dist','slope','elev')

#~ # merge pred_stack (without road_dist, urban_dist, slope, elev) and pred_stack_upd
#~ print('creating new pred_stack')
#~ pred_stack  <- subset(pred_stack,c('grid','protect','access','rsds'))
#~ pred_stack  <- extend(pred_stack,pred_stack_upd)	#extend to -90:90

#~ grid_km_upd        <- raster('/vol/milkundata/PowerSystem/final_distance.tif')
#~ grid_km_upd        <- extend(grid_km_upd,pred_stack_upd)	#extend to -90:90
#~ names(grid_km_upd) <- c('grid_km')

#~ pred_stack  <- stack(pred_stack,pred_stack_upd,grid_km_upd)

#~ print('creating regional pred_stack')
#~ pred_stack_reg <- mask(pred_stack,polygon_region)					# this masking / cropping is time consuming (~hour)
#~ pred_stack_reg <- crop(pred_stack_reg,polygon_region)

#~ print('creating regional ESA stack')
#~ ESA_stack_reg  <- mask(ESA_stack,polygon_region)	
#~ ESA_stack_reg  <- crop(ESA_stack_reg,polygon_region)

#~ predictors     <- stack(pred_stack_reg,ESA_stack_reg)

#~ # TO DO: temporarily store input stacks, for debugging
outputfilename_stack <- 'predictor_stack_Afr.gri'
#~ writeRaster(predictors,filename=outputfilename_stack,overwrite=TRUE,format='raster')

#~ print('stop script for now after creating outputfile raster')
#~ stop()

predictors <- stack(outputfilename_stack)

### creating presence raster and absence raster
### ------------------------------------------------------------------------------------------------------
wiki_sel_lonlat  <- subset(wiki_df,select=c('lon_wiki','lat_wiki'))             # tibble with XXX entries (i.e. 3432 for Eur)				
presence_raster  <- rasterize(wiki_sel_lonlat,predictors[['rsds']],field=1000)	# RasterLayer. Value of 1000 is randomly chosen to give a fixed

test_raster      <- predictors[['slope']]
test_raster[test_raster > 0 ] <- 1												# continuous 'background' from which to remove presences & water cells

water_raster     <- predictors[['Water']]
water_raster[water_raster > 0.9375 & water_raster < 1.1] <- 1000 				# kicking out grid cells with ONLY water (fractions are in steps of 0.0625 so 0.9375 is the last fraction below 1)

absence_raster   <- merge(presence_raster,water_raster,test_raster)
absence_raster[absence_raster == 1000] <- NA

nr_presences     <- nrow(wiki_sel_lonlat)
nr_absences      <- 10*nr_presences
abs_weight       <- dismo::randomPoints(absence_raster,nr_absences)
abs_df           <- as.data.frame(abs_weight)
print(paste('CHECK: dataframe of absences contains',nrow(abs_df),'rows / locations, should be', 10*nr_presences))

pdf(file='Plot_presences_absences_spatial.pdf')
ggplot() + theme_minimal() + 
geom_point(data=abs_df,aes(x=x,y=y),size=.5) +  geom_point(data=wiki_sel_lonlat, aes(x=lon_wiki, y=lat_wiki),color='red',size=.5) + labs(title = paste0("Utility-scale PV presences (red, ", nr_presences,") and absences (black, ", nr_absences,")"))
dev.off()

### get predictor values at presence and absence locations
### ------------------------------------------------------------------------------------------------------
values_P    <- raster::extract(predictors,wiki_sel_lonlat,df=TRUE)
values_A    <- raster::extract(predictors,abs_df,df=TRUE)
names(wiki_sel_lonlat)[names(wiki_sel_lonlat) == "lon_wiki"] <- "x"			# make sure column names are the same in order to merge df's
names(wiki_sel_lonlat)[names(wiki_sel_lonlat) == "lat_wiki"] <- "y"
abs_df$PA               = 'A'
wiki_sel_lonlat$PA      = 'P'
abs_df$PA_bool          = FALSE
wiki_sel_lonlat$PA_bool = TRUE
df_PA       <- rbind(cbind(wiki_sel_lonlat,values_P),cbind(abs_df,values_A))

print(paste0('Number of total cases: ',nrow(df_PA)))
df_NA <- df_PA[!complete.cases(df_PA),]
df_PA <- df_PA[complete.cases(df_PA),]					# NOTE: occurs either due to presences outside the polygon I use as mask, or mismatches between the polygon and Mirza's maps. TO DO: select predictor values from full maps, and only use polygons to select absences from? Somehow make sure to select absences that are both within polygons AND Mirza's maps?
print(paste0('Number of remaining cases: ',nrow(df_PA)))

#ggplot() + theme_minimal() + geom_point(data=abs_df,aes(x=x,y=y),size=.5) + geom_point(data=df_NA,aes(x=x,y=y,color=PA),size=1) 


### add country names
### -----------------------------------------------------------------------------------------------------
df_PA$lat_orig <- df_PA$y											#keep original lats and lons for plotting (overwritten by st_as_sf)
df_PA$lon_orig <- df_PA$x
df_PA_sf       <- st_as_sf(df_PA, coords = c("x", "y"), crs = 4326)

EEZ <- st_read('/vol/milkunarc/jbosmans/countries_EEZ/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp')
df_countryEEZ  <- st_join(df_PA_sf,EEZ, join = st_intersects)
df_noEEZ       <- subset(df_countryEEZ,is.na(SOVEREIGN1))
df_noEEZ_xy    <- subset(df_noEEZ,select=c('lat_orig','lon_orig'))
df_countryEEZ  <- subset(df_countryEEZ,!is.na(SOVEREIGN1))

print(paste0('Dataframe with ',nrow(df_PA), ' rows, of which ', nrow(df_countryEEZ), ' have a country name'))

df_country <- subset(df_countryEEZ,select=c("PA","PA_bool","ID", "road_dist", "grid", "access", "slope", "elev", "protect", "rsds",	 "Agriculture", "Forest", "Short_natural", "Wetland", "Urban", "Bare", "Water", "available_area", "lat_orig", "lon_orig", "SOVEREIGN1", "POL_TYPE", "Y_1","x_1","AREA_KM2","grid_km"))

df_country$continent <- countrycode(sourcevar=df_country$SOVEREIGN1, origin="country.name",destination="continent")
sum(is.na(df_country$continent))	# check that there are no NAs in continent

write_xlsx(df_country,outputfilename)

