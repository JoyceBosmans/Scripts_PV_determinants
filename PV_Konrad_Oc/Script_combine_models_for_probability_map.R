pacman::p_load(ggplot2, spData, sp, sf, raster,writexl,data.table,INLA,plyr)

dir_Konrad 						<- '/vol/milkunarc2/kmielke/pv/data/predictions/Oc' 
outputfilename_raster           <- 'Probability_map_Oc.gri'

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

fwrite(df_full,'df_Oc_full_predictions.csv')

### create Probability_map*gri
print('Create a raster out of df_full')
df_for_raster     <- subset(df_full,select=c('x_1','Y_1','prediction'))
prediction_raster <- rasterFromXYZ(df_for_raster)

#prediction_raster <- crop(prediction_raster,extent(-180,-50,10,90))

print('save raster for Script_compare_PoO_COE.R')
writeRaster(prediction_raster,filename=outputfilename_raster,overwrite=TRUE,format='raster')


#df_access: access, lat, lon created from df_all in Script_create_df_allgriddcels.R
#now have correct lat and lon to create prediction raster:

df_access <- read.csv('df_Oc_access.csv')
df_for_raster <- subset(df_access,select=c('x','y'))
df_for_raster$prediction <- df_full$prediction
prediction_raster <- rasterFromXYZ(df_for_raster)
mean(df_full$prediction)
sd(df_full$prediction)
mean(prediction_raster[],na.rm=TRUE)
sd(prediction_raster[],na.rm=TRUE)

print('save raster for Script_compare_PoO_COE.R')
writeRaster(prediction_raster,filename=outputfilename_raster,overwrite=TRUE,format='raster')
