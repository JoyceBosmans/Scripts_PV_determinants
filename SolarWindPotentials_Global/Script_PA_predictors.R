pacman::p_load(readxl, writexl, ggplot2, raster,dismo)

# inputs obtained from other scripts (see readme.txt)
pred_stack        <- stack('pred_stack_global.gri')
ESA_stack         <- stack('ESA_stack_2000.gri')
names(ESA_stack)  <- c('Agriculture','Forest','Short_natural','Wetland','Urban','Bare','Water','available_area')
wiki_df           <- read_excel('/vol/milkunB/jbosmans/WikiSolar/200929_RUN365_B99--.xlsx')

### presences: obtained from Wiki-Solar, get lat-lon, create raster ----------------------------------------------
print(paste('Number of PV facilities globally:',nrow(wiki_df)))
wiki_df <- wiki_df[wiki_df$Status %in% c('A','B'),]
print(paste('   of which',nrow(wiki_df),'are currently operating'))
wiki_df <- subset(wiki_df, !Pt_TcArr== 'Floating' | is.na(Pt_TcArr))	# delete floating PV
print(paste('   of which',nrow(wiki_df),'are non-floating'))

wiki_df <- wiki_df[complete.cases(wiki_df$"Lat,Long"),]     # delete locations without location
wiki_df <- tidyr::separate(data = wiki_df, col = "Lat,Long", into = c("lat_wiki", "lon_wiki"), sep = "\\,")
wiki_df <- transform(wiki_df,lon_wiki=as.numeric(lon_wiki))
wiki_df <- transform(wiki_df,lat_wiki=as.numeric(lat_wiki))

wiki_sel_lonlat  <- subset(wiki_df,select=c('lon_wiki','lat_wiki'))             # tibble with 4,468 entries (nr_facilities)				
print(paste('Number of PV facilities globally with given location:',nrow(wiki_sel_lonlat)))
presence_raster  <- rasterize(wiki_sel_lonlat,pred_stack[['rsds']],field=1000)	# RasterLayer. Value of 1000 is randomly chosen to give a fixed value to presences 
wiki_arraytype   <- subset(wiki_df,select=c('Pt_TcArr','Status'))				# check that its same order as wiki_sel_lonlat? Use to check array type against ESA land cover types (also urban_dist?)

### absences, selected from gridcells without presences and without water ----------------------------------------
test_raster <- pred_stack[['slope']]
test_raster[test_raster > 0 ] <- 1								# continuous 'background' from which to remove presences & water cells

water_raster    <- ESA_stack[['Water']]
water_raster[water_raster > 0.9375 & water_raster < 1.1] <- 1000 # kicking out grid cells with ONLY water (fractions are in steps of 0.0625 so 0.9375 is the last fraction below 1)

print('creating absence raster')
absence_raster  <- merge(presence_raster,water_raster,test_raster)
plot(absence_raster)
absence_raster[absence_raster == 1000] <- NA

print('selecting absences using randomPoints')
nr_presences <- nrow(wiki_sel_lonlat)
nr_absences  <- 3*nr_presences/0.77		# because randomPoints somehow generates 0.77 times less requested number of points
abs_weight   <- dismo::randomPoints(absence_raster,nr_absences)
abs_df       <- as.data.frame(abs_weight)
print(paste('CHECK: dataframe of absences contains',nrow(abs_df),'rows / locations, should be', nr_presences*3))
print('NOTE: manually adapted nr_absences because randomPoints generates less points than requested')

pdf(file='Plot_presences_absences_spatial.pdf')
ggplot() + theme_minimal() + 
geom_point(data=abs_df,aes(x=x,y=y),size=.5) +  geom_point(data=wiki_sel_lonlat, aes(x=lon_wiki, y=lat_wiki),color='red',size=.5) + labs(title = paste0("Utility-scale PV presences (red, ", nr_presences,") and absences (black, ", nr_absences,")"))
dev.off()

### get predictor values for all presence and absence locations ----------------------------------------------------
print('Note: using latitudinal extent from -57 to 84')
extent_glob <- extent(-180,180,-57,84)
ESA_cropped <- crop(ESA_stack,extent_glob)
predictors  <- stack(pred_stack,ESA_cropped)			#pred_stack lat -57:85, ESA: -90:90. should be fine as pred_stack includes all latitudes where land is present (except antarctica)
values_P    <- raster::extract(predictors,wiki_sel_lonlat,df=TRUE)
values_A    <- raster::extract(predictors,abs_df,df=TRUE)
names(wiki_sel_lonlat)[names(wiki_sel_lonlat) == "lon_wiki"] <- "x"			# make sure column names are the same in order to merge df's
names(wiki_sel_lonlat)[names(wiki_sel_lonlat) == "lat_wiki"] <- "y"
abs_df$PA               = 'A'
wiki_sel_lonlat$PA      = 'P'
abs_df$PA_bool          = FALSE
wiki_sel_lonlat$PA_bool = TRUE
df_PA      <- rbind(cbind(wiki_sel_lonlat,values_P),cbind(abs_df,values_A)) # dataframe with 34,121 rows / locations
df_P       <- cbind(wiki_sel_lonlat,values_P,wiki_arraytype)
df_P       <- df_P[complete.cases(df_P$urban),]								# dataframe with 10,305 rows / locations

# remove missing values (occuring only for ~31 cases out of 16,433)
print(paste0('Number of total cases: ',nrow(df_PA)))
df_NA <- df_PA[!complete.cases(df_PA),]
df_PA <- df_PA[complete.cases(df_PA),]					# NOTE: occurs either due to presences outside the polygon I use as mask, or mismatches between the polygon and Mirza's maps. TO DO: select predictor values from full maps, and only use polygons to select absences from? Somehow make sure to select absences that are both within polygons AND Mirza's maps?
print(paste0('Number of remaining cases: ',nrow(df_PA)))

write.csv(df_PA,'df_PA_global.csv')

# select those on rooftops, check distribution of land cover types versus overall facilities (no distinction between stati!)
roof     <- c('Rooftop','Carport-roof')
df_roof  <- df_P[df_P$Pt_TcArr %in% roof,]
print(paste('Number of facilities on (carport) rooftops:',nrow(df_roof)))
print(paste('Mean urban distance of rooftop facilities:',mean(df_roof$urban_dist)))
print(paste('Mean urban distance of all facilities:    ',mean(df_P$urban_dist)))
print(paste('Mean urban fraction of rooftop facilities:',mean(df_roof$Urban)))
print(paste('Mean urban fraction of all facilities:    ',mean(df_P$Urban)))
# somehow test if difference is significant? quantile(df_roof$Urban,probs=c(0.025,0.5,0.975))

#~ # check differences between current (status A,B) and planned (status C) facilities (NOTE: C now already deleted above)
#~ df_cur <- df_P[df_P$Status %in% c('A','B'),]
#~ df_fut <- df_P[df_P$Status %in% c('C'),]
#~ print(paste('Current facilities:',nrow(df_cur),', Planned facilities:',nrow(df_fut)))
#~ print(paste('Mean urban fraction of current facilities:',mean(df_cur$Urban)))
#~ print(paste('Mean urban fraction of future facilities: ',mean(df_fut$Urban)))
#~ print(paste('Mean agriculture fraction of current facilities:',mean(df_cur$Agriculture)))
#~ print(paste('Mean agriculture fraction of future facilities: ',mean(df_fut$Agriculture)))
#~ h1 <-  hist(df_cur$Urban,breaks=35)
#~ h2 <-  hist(df_fut$Urban,breaks=35)
#~ plot( h1, col=rgb(0,0,1,1/4), xlim=c(0,1),main='Urban fraction current (blue) and planned (pink) facilities')
#~ plot( h2, col=rgb(1,0,0,1/4), xlim=c(0,1), add=T)
