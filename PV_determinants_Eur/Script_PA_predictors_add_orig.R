pacman::p_load(readxl, writexl, ggplot2, raster,dismo,sf,countrycode)

### updated predictors (road_dist, slope, elevation) already added in Script_PA_predictors. Here add grid_km 

df_PA <- read_excel('/vol/milkunB/jbosmans/PV_determinants_Eur/df_PA_Eur_country_261121.xlsx')
outputfilename <- 'df_PA_Eur_country_261121.xlsx'

pred_stack_upd <- stack('/vol/milkunB/jbosmans/PV_determinants_global/predictor_stack_global_251121.gri')	# road_dist, slope, elev
names(pred_stack_upd) <- c('road_dist','slope','elev')

# also add grid distance in m instead of degree (see PV*global/Script_input_grid.R)
grid_km_upd        <- raster('/vol/milkunB/jbosmans/PV_determinants_global/grid_km.gri')
grid_km_upd        <- extend(grid_km_upd,pred_stack_upd)	#extend to -90:90
names(grid_km_upd) <- c('grid_km')

pred_stack_upd <- grid_km_upd

# check distribution of P and A as a safety check of lats and lons: 
ggplot() + theme_minimal() + geom_point(data=df_PA,aes(x=lon_orig,y=lat_orig,color=PA),size=0.5) 

# get lats / lons of presences and absences, select new predictors, add to df:
sel_lonlat  <- subset(df_PA,select=c('lon_orig','lat_orig'))
values_PA   <- raster::extract(pred_stack_upd,sel_lonlat,df=TRUE)

names(df_PA)[names(df_PA) == "grid"]      <- "grid_old"

df_PA <- cbind(df_PA,values_PA)
nrow(df_PA)
df_PA = df_PA[,!(names(df_PA) %in% c('geometry'))]
df_PA <- df_PA[complete.cases(df_PA),]
nrow(df_PA)

ggplot() + theme_minimal() + geom_point(data=df_PA,aes(x=grid_old,y=grid_km,color=lat_orig),size=0.5) 

write_xlsx(df_PA,outputfilename)

