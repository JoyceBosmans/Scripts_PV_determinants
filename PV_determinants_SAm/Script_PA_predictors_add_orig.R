pacman::p_load(readxl, writexl, ggplot2, raster,dismo,sf,countrycode)

### add updated predictors (road_dist, slope, elevation) to existing dataframe 

df_PA <- read_excel('/vol/milkunB/jbosmans/SolarWindPotentials_SAm/df_PA_SAm_country_281021.xlsx')
outputfilename <- 'df_PA_SAm_country_261121.xlsx'

pred_stack_upd <- stack('/vol/milkunB/jbosmans/PV_determinants_global/predictor_stack_global_251121.gri')	# road_dist, slope, elev
names(pred_stack_upd) <- c('road_dist','slope','elev')

# check distribution of P and A as a safety check of lats and lons: 
ggplot() + theme_minimal() + geom_point(data=df_PA,aes(x=lon_orig,y=lat_orig,color=PA),size=0.5) 

grid_km_upd        <- raster('/vol/milkunB/jbosmans/PV_determinants_global/grid_km.gri')
grid_km_upd        <- extend(grid_km_upd,pred_stack_upd)	#extend to -90:90
names(grid_km_upd) <- c('grid_km')

pred_stack_upd <- stack(pred_stack_upd,grid_km_upd)

# get lats / lons of presences and absences, select new predictors, add to df:
sel_lonlat  <- subset(df_PA,select=c('lon_orig','lat_orig'))
values_PA   <- raster::extract(pred_stack_upd,sel_lonlat,df=TRUE)

colnames(df_PA)
names(df_PA)[names(df_PA) == "road_dist"] <- "road_dist_old"
names(df_PA)[names(df_PA) == "slope"]     <- "slope_old"
names(df_PA)[names(df_PA) == "elev"]      <- "elev_old"
names(df_PA)[names(df_PA) == "grid"]      <- "grid_old"


df_PA <- cbind(df_PA,values_PA)
nrow(df_PA)
df_PA = df_PA[,!(names(df_PA) %in% c('geometry'))]
df_PA <- df_PA[complete.cases(df_PA),]
nrow(df_PA)

write_xlsx(df_PA,outputfilename)

