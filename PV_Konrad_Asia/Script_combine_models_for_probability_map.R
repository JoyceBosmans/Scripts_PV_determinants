pacman::p_load(ggplot2, spData, sp, sf, raster,writexl,data.table,INLA,plyr)

dir_Konrad 						<- '/vol/milkunarc2/kmielke/pv/data/predictions/Asia' 
outputfilename_raster           <- 'Probability_map_Asia.gri'
outputfilename_raster_nocountry <- 'Probability_map_Asia_fixed.gri'
outputfilename_df				<- 'df_Asia_all_probability.csv'
outputfilename_df_360			<- 'df_Asia_all_probability_360.csv'

# write loop to open each model, get predictions, add to df, merge to df_full
print(paste('Obtaining files from',dir_Konrad))
nRuns <- length(list.files(dir_Konrad))

load(file.path(dir_Konrad,paste0(1,'.Rda')))
df_full <- df_full_temp

for (i in 2:nRuns) {
	load(file.path(dir_Konrad,paste0(i,'.Rda')))
	df_full <- rbind(df_full,df_full_temp)
	#print(paste(dim(df_full)[1],dim(df_full_temp)[1]))
	print(paste('Added part',i,'of ',nRuns,'. Length of df_full now',nrow(df_full),'mean prediction:',mean(df_full$prediction)))
}

# store output as dataframe (as a check, raster seems to miss everything south of equator)
fwrite(df_full,outputfilename_df)

print(colnames(df_full))
print('latitude range:')
print(paste(min(df_full$lat_orig),max(df_full$lat_orig)))
print('longitude range:')
print(paste(min(df_full$lon_orig),max(df_full$lon_orig)))

df_full$lon_360 <- df_full$lon_orig + 180	#shift from -180:180 to 0:360
df_360          <- subset(df_full,select=c('lon_360','lat_orig','prediction'))
fwrite(df_360,outputfilename_df_360)
  
### create Probability_map*gri
print('Create a raster out of df_full')
df_for_raster     <- subset(df_full,select=c('lon_orig','lat_orig','prediction'))
prediction_raster <- rasterFromXYZ(df_for_raster)

#prediction_raster <- crop(prediction_raster,extent(-180,-50,10,90))

print('save raster for Script_compare_PoO_COE.R')
writeRaster(prediction_raster,filename=outputfilename_raster,overwrite=TRUE,format='raster')
