pacman::p_load(readxl, writexl, ggplot2, raster,dismo,sf,countrycode)

### updated predictors (road_dist, slope, elevation) already added in Script_PA_predictors. Here add grid_km 

df_PA <- read_excel('/vol/milkunB/jbosmans/PV_determinants_Asia/df_PA_Asia_country_060622.xlsx')
outputfilename <- 'df_PA_Asia_country_060622.xlsx'

pred_stack_upd <- stack('/vol/milkunB/jbosmans/PV_determinants_global/predictor_stack_global_251121.gri')	# road_dist, slope, elev
names(pred_stack_upd) <- c('road_dist','slope','elev')

# also add grid distance in m instead of degree (see PV*global/Script_input_grid.R)
#grid_km_upd       <- raster('/vol/milkunB/jbosmans/PV_determinants_global/grid_km.gri')
grid_km_upd        <- raster('/vol/milkundata/PowerSystem/final_distance.tif')
grid_km_upd        <- extend(grid_km_upd,pred_stack_upd)	#extend to -90:90
names(grid_km_upd) <- c('grid_km')

pred_stack_upd <- grid_km_upd

# check distribution of P and A as a safety check of lats and lons: 
df_PA$lon_new <- df_PA$lon_orig
df_PA$lon_new[df_PA$lon_new<0] <- df_PA$lon_new[df_PA$lon_new<0] + 360
df_P <- df_PA[df_PA$PA=='P',]
df_A <- df_PA[df_PA$PA=='A',]
ggplot() + theme_minimal() + geom_point(data=df_A,aes(x=lon_new,y=lat_orig),size=0.2) + geom_point(data=df_P,aes(x=lon_new,y=lat_orig),size=0.5,color='red') + labs(y= "y", x = "x")
 
# get lats / lons of presences and absences, select new predictors, add to df:
sel_lonlat  <- subset(df_PA,select=c('lon_orig','lat_orig'))
values_PA   <- raster::extract(pred_stack_upd,sel_lonlat,df=TRUE)


names(df_PA)[names(df_PA) == "grid"]      <- "grid_old"

df_PA <- cbind(df_PA,values_PA)
nrow(df_PA)
df_PA = df_PA[,!(names(df_PA) %in% c('geometry'))]
df_PA <- df_PA[complete.cases(df_PA),]
nrow(df_PA)

write_xlsx(df_PA,outputfilename)

ggplot() + theme_minimal() + geom_point(data=df_PA,aes(x=lon_orig,y=lat_orig,color=grid_km,shape=PA),show.legend=TRUE,size=0.05) +scale_color_gradientn(colours = rainbow(6))
