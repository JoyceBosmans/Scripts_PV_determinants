pacman::p_load(ggplot2, spData, sp, sf, raster,writexl,data.table,readxl,INLA)

### set output file names, dirs etc
df_Eur_allgridcells				<- 'df_Eur_allgridcells.csv'
outputfilename_raster           <- 'Probability_map_Eur.gri'
outputfilename_raster_nocountry <- 'Probability_map_Eur_fixed.gri'
best_model						<- '3373'
dir_Konrad 						<- '/vol/milkunarc2/kmielke/pv' 
load(file = file.path(dir_Konrad,'/data/rda/df_PA_Eur_country_060622_proprocessed.Rda'))
EEZ <- st_read('/vol/milkunarc/jbosmans/countries_EEZ/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp')

### if updated file already present in this dir, load and go ahead. Otherwise add updated grid_km, store, then continue.
if(file.exists(df_Eur_allgridcells)) {
  print('loading dataframe with all grid cells')
  df_full <- read.csv(df_Eur_allgridcells)
} else {
  print('updating dataframe with grid_km and storing in this dir')
  ### dataframe with all grid cells already created in ../PV_determinants_Eur/Script_create_probability_maps.R
  df_full <- read.csv(paste0('../PV_determinants_Eur/',df_Eur_allgridcells))

  ### add updated grid distance computations
  grid_km_upd        <- raster('/vol/milkundata/PowerSystem/final_distance.tif')
  names(grid_km_upd) <- c('grid_km')
  sel_lonlat  	     <- subset(df_full,select=c('lon_orig','lat_orig'))
  values_grid_km 	 <- raster::extract(grid_km_upd,sel_lonlat,df=TRUE)
  df_full$grid_orig  <- df_full$grid_km
  df_full$grid_km    <- values_grid_km$grid_km 

  df_full            <- df_full[complete.cases(df_full$grid_km),]	# 496 out of 12236558 dont have grid_km
  
  # somehow for Europe country names (SOVEREIGN1) are missing so add these
  df_full$y          <- df_full$lat_orig											
  df_full$x          <- df_full$lon_orig
  df_full_sf         <- st_as_sf(df_full, coords = c("x", "y"), crs = 4326)

  df_full            <- st_join(df_full_sf,EEZ, join = st_intersects)

  df_full            <- subset(df_full,select=c('grid','protect','access','rsds','road_dist','slope','elev','Agriculture','Forest','Short_natural','Wetland','Urban','Bare','Water','available_area','lat_orig','lon_orig','SOVEREIGN1','grid_km', 'grid_orig','Y_1.x','Y_1.y','x_1.x','x_1.y'))


  fwrite(df_full,df_Eur_allgridcells)	# not yet with lambert coordinats. Add those when creating stk.pred
}

### load the model
load(paste0(best_model,'.Rda'))
model_performance <- read.csv(file.path(dir_Konrad,'/results/waic_EurRev.txt'), sep = ' ', header = FALSE)
print(paste('check that best model is ',best_model,':',model_performance[which.min(model_performance$V2), 1]))


### create a stack of data used for fitting, based on df_PA (presences and absences, not all grid cells)
# create matrix of coordinate pairs and ranges
coords     = cbind(df_PA$x_coord, df_PA$y_coord)
x_range    = max(coords[,1]) - min(coords[,1])
y_range    = max(coords[,2]) - min(coords[,2])
mean_range = (x_range + y_range)/2

# create mesh and helping structure A
mesh <- inla.mesh.2d(loc = coords, max.edge=c(1,2)*mean_range/5, cutoff = c(mean_range/25))
A    <- inla.spde.make.A(mesh=mesh,loc=as.matrix(coords))

# create spatial structure spde, and helping structure iset, set seed
spde <- inla.spde2.matern(mesh, alpha=1.5)
iset <- inla.spde.make.index(name = "spatial.field", spde$n.spde)
#set.seed(model_performance[best_model,3 ])

# use df_PA as train data and create stack used for fitting (df_full will be used for stk.pred)
trainFrame <- df_PA
stk.fit <- inla.stack(data=list(y = as.integer(trainFrame$PA_bool, n = length(trainFrame$PA_bool))),
                            effects=list(c(list(Intercept=1), iset),
                                           list(SOVEREIGN1 = trainFrame$SOVEREIGN1,
                                                travel_log = trainFrame$travel_log,
                                                slope_log = trainFrame$slope_log,
                                                elev_log = trainFrame$elev_log,
                                                #road_log = trainFrame$road_log,
                                                #grid_log = trainFrame$grid_log,
                                                protect = trainFrame$protect,
                                                rsds = trainFrame$rsds,
                                                Agriculture = trainFrame$Agriculture,
                                                Short_natural = trainFrame$Short_natural,
                                                #Wetland = trainFrame$Wetland,
                                                Urban = trainFrame$Urban
                                        )
                                    ), 
                            A=list(A,1),
                            tag='fit')

### create a stack of data used for predicting, based on df_full# create matrix of coordinate pairs and ranges

# first do coordinate transformation
d               <- data.frame(lon=df_full$lon_orig, lat=df_full$lat_orig)
coordinates(d)  <- c("lon", "lat")
proj4string(d)  <- CRS("+init=epsg:4326") # WGS 84
CRS.new         <- CRS("+init=esri:102014") # Lambert projection
d_transformed   <- spTransform(d, CRS.new)
df_full$x_coord <- d_transformed$lon
df_full$y_coord <- d_transformed$lat


# log-transform a subset of variables and normalize all (from /vol/milkunarc2/kmielke/pv/code/preprocessing_Eur.R)
df_full$travel_log <- log10(df_full$access + 1)
df_full$slope_log  <- log10(df_full$slope)
df_full$elev_log   <- log10(df_full$elev + abs(min(df_full$elev))+1)
df_full$road_log   <- log10(df_full$road_dist + 1)

df_full$travel_log    <- (df_full$travel_log - mean(df_full$travel_log))/sd(df_full$travel_log)
df_full$slope_log     <- (df_full$slope_log - mean(df_full$slope_log))/sd(df_full$slope_log)
df_full$elev_log      <- (df_full$elev_log - mean(df_full$elev_log))/sd(df_full$elev_log)
df_full$road_log      <- (df_full$road_log - mean(df_full$road_log))/sd(df_full$road_log)
df_full$rsds          <- (df_full$rsds - mean(df_full$rsds))/sd(df_full$rsds)
df_full$protect       <- (df_full$protect - mean(df_full$protect))/sd(df_full$protect)
df_full$Agriculture   <- (df_full$Agriculture - mean(df_full$Agriculture))/sd(df_full$Agriculture)
df_full$Short_natural <- (df_full$Short_natural - mean(df_full$Short_natural))/sd(df_full$Short_natural)
df_full$Wetland       <- (df_full$Wetland - mean(df_full$Wetland))/sd(df_full$Wetland)
df_full$Urban         <- (df_full$Urban - mean(df_full$Urban))/sd(df_full$Urban)
df_full$Bare          <- (df_full$Bare - mean(df_full$Bare))/sd(df_full$Bare)
df_full$Water         <- (df_full$Water - mean(df_full$Water))/sd(df_full$Water)

# set coordinates 
coords_full     = cbind(df_full$x_coord, df_full$y_coord)
#x_range_full    = max(coords_full[,1]) - min(coords_full[,1])
#y_range_full    = max(coords_full[,2]) - min(coords_full[,2])
#mean_range_full = (x_range_full + y_range_full)/2

# create mesh and helping structure A
#mesh_full <- inla.mesh.2d(loc = coords_full, max.edge=c(1,2)*mean_range_full/5, cutoff = c(mean_range_full/25))
A_full    <- inla.spde.make.A(mesh=mesh,loc=as.matrix(coords_full))

# create spatial structure spde, and helping structure iset, set seed
#spde_full <- inla.spde2.matern(mesh_full, alpha=1.5)
#iset_full <- inla.spde.make.index(name = "spatial.field", spde_full$n.spde)
#set.seed(model_performance[best_model,3 ])

# use df_full as test data and create stack used for predicting. Using iset and A_full
testFrame <- df_full
stk.pred  <- inla.stack(data=list(y = rep(NA, length(testFrame[,1])), n = length(testFrame[,1])),
                            effects=list(c(list(Intercept=1), iset),
                                           list(SOVEREIGN1 = testFrame$SOVEREIGN1,
                                                travel_log = testFrame$travel_log,
                                                slope_log = testFrame$slope_log,
                                                elev_log = testFrame$elev_log,
                                                protect = testFrame$protect,
                                                rsds = testFrame$rsds,
                                                Agriculture = testFrame$Agriculture,
                                                Short_natural = testFrame$Short_natural,
                                                Urban = testFrame$Urban
                                        )
                                    ), 
                            A=list(A_full,1),
                            tag='pred')

# stk.full has stk.fit and stk.pred
stk.full <- inla.stack(stk.fit, stk.pred)

# using predictors from the best model (see Script_find_best_model.R)
formula <- y ~ 1 + travel_log + slope_log + elev_log + protect + rsds + Agriculture + Short_natural + Urban + f(SOVEREIGN1, model = "iid") + f(spatial.field, model = spde)

# set seed
set.seed(model_performance[best_model,3 ])

# build the model - this overwrites the model loaded from the Rda file? (best model found through Script_find_best_model.R?)
model <-inla(formula,
                data=inla.stack.data(stk.full,spde=spde), # data plus spde
                family= 'binomial', # data family
                control.family = list(link = "logit"), # logit for logistic regression
                control.predictor=list(link = 1, A=inla.stack.A(stk.full)), # calculate the covariate weights
                control.compute = list(waic = TRUE) # calculate Watanabe-Akaike information criterion 
                )
                
summary(model)	# same as in Script_find_best_model.R

save(stk.full,file='stk.full.1jul.Rda')	# for testing (could be loaded without having to rerun the rest of the script)
save(model,file='model_full.1jul.Rda')

# get the predictions - use tag = pred?
index       <- inla.stack.index(stack = stk.full, tag = "pred")$data
predictions <- model$summary.fitted.values[index, "mean"]
mean(predictions)
sum(is.na(predictions))

# save df_full with predictions column, also predictions as a raster? check what is used for plotting and IMAGE comparison

