pacman::p_load(ggplot2, writexl,data.table,readxl,INLA,pROC)

best_model <- '10398'
dir_Konrad <- '/vol/milkunarc2/kmielke/pv' 
load('tab_coef_Afr.Rda')
tab_coef <- tab_coef_Afr

# load df_PA (data frame), in which predictors are already log-transformed and standardized
load(file = file.path(dir_Konrad,'/data/rda/df_PA_Afr_country_proprocessed.Rda'))
df_PA_orig <- read_xlsx('../PV_determinants_Afr/df_PA_Afr_country.xlsx')

# load model (copied to this dir in Script_find_best_model)
load(paste0(best_model,'.Rda'))

##### compute pseudoR2 - first some general modelling
model_performance <- read.csv(file.path(dir_Konrad,'/waic_AfrRev.txt'), sep = ' ', header = FALSE)
print(paste('check that best model is ',best_model,':',model_performance[which.min(model_performance$V2), 1]))

# create matrix of coordinate pairs
coords = cbind(df_PA$x_coord, df_PA$y_coord)

# calculate range of x and y directions
x_range = max(coords[,1]) - min(coords[,1])
y_range = max(coords[,2]) - min(coords[,2])

# average ranges
mean_range = (x_range + y_range)/2

# create mesh
mesh <- inla.mesh.2d(loc = coords, max.edge=c(1,2)*mean_range/5, cutoff = c(mean_range/25))

# A is a helping structure to map the data points onto the mesh
A <- inla.spde.make.A(mesh=mesh,loc=as.matrix(coords))

# spde is the spatial structure, here I chose a matern structure with alpha = 1.5 which is an exponential decay
spde <- inla.spde2.matern(mesh, alpha=1.5)

# another helping structure, this gives an index to each position of the mesh
iset <- inla.spde.make.index(name = "spatial.field", spde$n.spde)

# set random seed    
set.seed(model_performance[best_model,3 ])

# split in train and test data
trainFrame <- df_PA
testFrame <- df_PA

# create stack of data used for fitting
stk.fit <- inla.stack(data=list(y = as.integer(trainFrame$PA_bool, n = length(trainFrame$PA_bool))),
                            effects=list(c(list(Intercept=1), iset),
                                           list(SOVEREIGN1 = trainFrame$SOVEREIGN1,
                                                travel_log = trainFrame$travel_log,
                                                slope_log = trainFrame$slope_log,
                                                elev_log = trainFrame$elev_log,
                                                grid_log = trainFrame$grid_log,
                                                road_log = trainFrame$road_log, 
                                                protect = trainFrame$protect,
                                                rsds = trainFrame$rsds,
                                                Agriculture = trainFrame$Agriculture,
                                                Short_natural = trainFrame$Short_natural,
                                                Wetland = trainFrame$Wetland,
                                                Urban = trainFrame$Urban,
                                                Bare = trainFrame$Bare,
                                                Water = trainFrame$Water
                                        )
                                    ), 
                            A=list(A,1),
                            tag='fit')

# get the predictions for training data - pR2 for original model - as a check                  
index <- inla.stack.index(stack = stk.fit, tag = "fit")$data
predictions <- model$summary.fitted.values[index, "mean"]
ground_truth <- as.integer(trainFrame$PA_bool)
mean_ground_truth <- mean(ground_truth)

pseudoRsquared <- 1 - (sum((ground_truth - predictions)^2)/sum((ground_truth - rep(mean_ground_truth))^2))
pseudoRsquared

formula <- y ~ 1 + travel_log + slope_log + elev_log + grid_log + road_log + protect + rsds + Agriculture + Short_natural + Wetland + Urban + Bare + Water + f(SOVEREIGN1, model = "iid") + f(spatial.field, model = spde)

#-------------------------------------------------------------------------------------------------------
# importance of travel_log - create stack with randomized values
if (tab_coef['travel_log',] != 0){
  stk.pred <- inla.stack(data=list(y = rep(NA, length(testFrame[,1])), n = length(testFrame[,1])),
                            effects=list(c(list(Intercept=1), iset),
                                           list(SOVEREIGN1 = testFrame$SOVEREIGN1,
                                                travel_log = sample(testFrame$travel_log,replace=FALSE),
                                                slope_log = testFrame$slope_log,
                                                elev_log = testFrame$elev_log,
                                                grid_log = testFrame$grid_log,
                                                road_log = testFrame$road_log, 
                                                protect = testFrame$protect,
                                                rsds = testFrame$rsds,
                                                Agriculture = testFrame$Agriculture,
                                                Short_natural = testFrame$Short_natural,
                                                Wetland = testFrame$Wetland,
                                                Urban = testFrame$Urban,
                                                Bare = testFrame$Bare,
                                                Water = testFrame$Water
                                        )
                                    ), 
                            A=list(A,1),
                            tag='pred')

  # stk.full has stk.fit and stk.pred
  stk.full <- inla.stack(stk.fit, stk.pred)

  # build the model - this overwrites the model loaded from the Rda file? (best model found through Script_find_best_model.R?)
  model <-inla(formula,
                data=inla.stack.data(stk.full,spde=spde), # data plus spde
                family= 'binomial', # data family
                control.family = list(link = "logit"), # logit for logistic regression
                control.predictor=list(link = 1, A=inla.stack.A(stk.full)), # calculate the covariate weights
                control.compute = list(waic = TRUE) # calculate Watanabe-Akaike information criterion 
                )

  index_pred          <- inla.stack.index(stack = stk.full, tag = "pred")$data
  predictions_pred    <- model$summary.fitted.values[index_pred, "mean"]
#~   #predictions_pred[is.na(predictions_pred)] <- 0.5	#not sure why NAs appear in predictions_pred.... replace with best guess (0.5)
#~   #replacing with 0.5 or removing NA's altogether (lines below) results in negative R2 (pseduRsquared_pred) and quite high PI? 
#~   temp                <- predictions_pred
#~   temp[!is.na(temp)]  <- 0 
#~   ground_truth_temp   <- temp * ground_truth
#~   predictions_pred    <- na.omit(predictions_pred)
#~   ground_truth_temp   <- na.omit(ground_truth_temp)
  
  pseudoRsquared_pred <- 1 - (sum((ground_truth - predictions_pred)^2)/sum((ground_truth - rep(mean_ground_truth))^2))
  PI_travel_log       <- 1 - cor(predictions,predictions_pred,method='spearman')
  print(paste('pR2:',pseudoRsquared,'pR2 random travel:',pseudoRsquared_pred,'PI travel:',PI_travel_log))
  sign                <- if(sign(tab_coef['travel_log',]) < 0) '#cc3300' else '#3399FF'
  df_PI_travel        <- data.frame('Travel times',PI_travel_log,sign)
  names(df_PI_travel) <- c('predictor','PI','sign')
} else {
  df_PI_travel        <- data.frame('Travel times',0,0)
  names(df_PI_travel) <- c('predictor','PI','sign')
}

#-------------------------------------------------------------------------------------------------------
# importance of slope_log - create stack with randomized values
if (tab_coef['slope_log',] != 0){
  stk.pred <- inla.stack(data=list(y = rep(NA, length(testFrame[,1])), n = length(testFrame[,1])),
                            effects=list(c(list(Intercept=1), iset),
                                           list(SOVEREIGN1 = testFrame$SOVEREIGN1,
                                                travel_log = testFrame$travel_log,
                                                slope_log = sample(testFrame$slope_log,replace=FALSE),
                                                elev_log = testFrame$elev_log,
                                                grid_log = testFrame$grid_log,
                                                road_log = testFrame$road_log, 
                                                protect = testFrame$protect,
                                                rsds = testFrame$rsds,
                                                Agriculture = testFrame$Agriculture,
                                                Short_natural = testFrame$Short_natural,
                                                Wetland = testFrame$Wetland,
                                                Urban = testFrame$Urban,
                                                Bare = testFrame$Bare,
                                                Water = testFrame$Water
                                        )
                                    ), 
                            A=list(A,1),
                            tag='pred')
                            
  # stk.full has stk.fit and stk.pred
  stk.full <- inla.stack(stk.fit, stk.pred)

  # build the model - this overwrites the model loaded from the Rda file? (best model found through Script_find_best_model.R?)
  model <-inla(formula,
                data=inla.stack.data(stk.full,spde=spde), # data plus spde
                family= 'binomial', # data family
                control.family = list(link = "logit"), # logit for logistic regression
                control.predictor=list(link = 1, A=inla.stack.A(stk.full)), # calculate the covariate weights
                control.compute = list(waic = TRUE) # calculate Watanabe-Akaike information criterion 
                )

  index_pred          <- inla.stack.index(stack = stk.full, tag = "pred")$data
  predictions_pred    <- model$summary.fitted.values[index_pred, "mean"]
#~   predictions_pred[is.na(predictions_pred)] <- 0.5
  pseudoRsquared_pred <- 1 - (sum((ground_truth - predictions_pred)^2)/sum((ground_truth - rep(mean_ground_truth))^2))
  PI_slope_log        <- 1 - cor(predictions,predictions_pred,method='spearman')
  print(paste('pR2:',pseudoRsquared,'pR2 random slope:',pseudoRsquared_pred,'PI slope:',PI_slope_log))

  sign                <- if(sign(tab_coef['slope_log',]) < 0) '#cc3300' else '#3399FF'	
  df_PI_slope         <- data.frame('Slope',PI_slope_log,sign)
  names(df_PI_slope)  <- c('predictor','PI','sign')
} else {
  df_PI_slope         <- data.frame('Slope',0,0)
  names(df_PI_slope)  <- c('predictor','PI','sign')
}

#-------------------------------------------------------------------------------------------------------
# importance of elev_log - create stack with randomized values
if (tab_coef['elev_log',] != 0){
  stk.pred <- inla.stack(data=list(y = rep(NA, length(testFrame[,1])), n = length(testFrame[,1])),
                            effects=list(c(list(Intercept=1), iset),
                                           list(SOVEREIGN1 = testFrame$SOVEREIGN1,
                                                travel_log = testFrame$travel_log,
                                                slope_log = testFrame$slope_log,
                                                elev_log = sample(testFrame$elev_log,replace=FALSE),
                                                grid_log = testFrame$grid_log,
                                                road_log = testFrame$road_log, 
                                                protect = testFrame$protect,
                                                rsds = testFrame$rsds,
                                                Agriculture = testFrame$Agriculture,
                                                Short_natural = testFrame$Short_natural,
                                                Wetland = testFrame$Wetland,
                                                Urban = testFrame$Urban,
                                                Bare = testFrame$Bare,
                                                Water = testFrame$Water
                                        )
                                    ), 
                            A=list(A,1),
                            tag='pred')
                            
  # stk.full has stk.fit and stk.pred
  stk.full <- inla.stack(stk.fit, stk.pred)

  # build the model - this overwrites the model loaded from the Rda file? (best model found through Script_find_best_model.R?)
  model <-inla(formula,
                data=inla.stack.data(stk.full,spde=spde), # data plus spde
                family= 'binomial', # data family
                control.family = list(link = "logit"), # logit for logistic regression
                control.predictor=list(link = 1, A=inla.stack.A(stk.full)), # calculate the covariate weights
                control.compute = list(waic = TRUE) # calculate Watanabe-Akaike information criterion 
                )

  index_pred          <- inla.stack.index(stack = stk.full, tag = "pred")$data
  predictions_pred    <- model$summary.fitted.values[index_pred, "mean"]
#~   predictions_pred[is.na(predictions_pred)] <- 0.5
  pseudoRsquared_pred <- 1 - (sum((ground_truth - predictions_pred)^2)/sum((ground_truth - rep(mean_ground_truth))^2))
  PI_elev_log         <- 1 - cor(predictions,predictions_pred,method='spearman')
  print(paste('pR2:',pseudoRsquared,'pR2 random elev:',pseudoRsquared_pred,'PI elev:',PI_elev_log))

  sign                <- if(sign(tab_coef['elev_log',]) < 0) '#cc3300' else '#3399FF'	
  df_PI_elev          <- data.frame('Elevation',PI_elev_log,sign)
  names(df_PI_elev)   <- c('predictor','PI','sign')
} else {
  df_PI_elev          <- data.frame('Elevation',0,0)
  names(df_PI_elev)   <- c('predictor','PI','sign')
}

#-------------------------------------------------------------------------------------------------------
# importance of road_log - create stack with randomized values
if (tab_coef['road_log',] != 0){
  stk.pred <- inla.stack(data=list(y = rep(NA, length(testFrame[,1])), n = length(testFrame[,1])),
                            effects=list(c(list(Intercept=1), iset),
                                           list(SOVEREIGN1 = testFrame$SOVEREIGN1,
                                                travel_log = testFrame$travel_log,
                                                slope_log = testFrame$slope_log,
                                                elev_log = testFrame$elev_log,
                                                grid_log = testFrame$grid_log,
                                                road_log = sample(testFrame$road_log,replace=FALSE), 
                                                protect = testFrame$protect,
                                                rsds = testFrame$rsds,
                                                Agriculture = testFrame$Agriculture,
                                                Short_natural = testFrame$Short_natural,
                                                Wetland = testFrame$Wetland,
                                                Urban = testFrame$Urban,
                                                Bare = testFrame$Bare,
                                                Water = testFrame$Water
                                        )
                                    ), 
                            A=list(A,1),
                            tag='pred')
                            
  # stk.full has stk.fit and stk.pred
  stk.full <- inla.stack(stk.fit, stk.pred)

  # build the model - this overwrites the model loaded from the Rda file? (best model found through Script_find_best_model.R?)
  model <-inla(formula,
                data=inla.stack.data(stk.full,spde=spde), # data plus spde
                family= 'binomial', # data family
                control.family = list(link = "logit"), # logit for logistic regression
                control.predictor=list(link = 1, A=inla.stack.A(stk.full)), # calculate the covariate weights
                control.compute = list(waic = TRUE) # calculate Watanabe-Akaike information criterion 
                )

  index_pred          <- inla.stack.index(stack = stk.full, tag = "pred")$data
  predictions_pred    <- model$summary.fitted.values[index_pred, "mean"]
#~   predictions_pred[is.na(predictions_pred)] <- 0.5
  pseudoRsquared_pred <- 1 - (sum((ground_truth - predictions_pred)^2)/sum((ground_truth - rep(mean_ground_truth))^2))
  PI_road_log         <- 1 - cor(predictions,predictions_pred,method='spearman')
  print(paste('pR2:',pseudoRsquared,'pR2 random elev:',pseudoRsquared_pred,'PI road:',PI_road_log))

  sign                <- if(sign(tab_coef['road_log',]) < 0) '#cc3300' else '#3399FF'	
  df_PI_road          <- data.frame('Road distance',PI_road_log,sign)
  names(df_PI_road)   <- c('predictor','PI','sign')
} else {
  df_PI_road          <- data.frame('Road distance',0,0)
  names(df_PI_road)   <- c('predictor','PI','sign')
}

#-------------------------------------------------------------------------------------------------------
# importance of grid_log - create stack with randomized values
if (tab_coef['grid_log',] != 0){
  stk.pred <- inla.stack(data=list(y = rep(NA, length(testFrame[,1])), n = length(testFrame[,1])),
                            effects=list(c(list(Intercept=1), iset),
                                           list(SOVEREIGN1 = testFrame$SOVEREIGN1,
                                                travel_log = testFrame$travel_log,
                                                slope_log = testFrame$slope_log,
                                                elev_log = testFrame$elev_log,
                                                grid_log = sample(testFrame$grid_log,replace=FALSE),
                                                road_log = testFrame$road_log, 
                                                protect = testFrame$protect,
                                                rsds = testFrame$rsds,
                                                Agriculture = testFrame$Agriculture,
                                                Short_natural = testFrame$Short_natural,
                                                Wetland = testFrame$Wetland,
                                                Urban = testFrame$Urban,
                                                Bare = testFrame$Bare,
                                                Water = testFrame$Water
                                        )
                                    ), 
                            A=list(A,1),
                            tag='pred')

  # stk.full has stk.fit and stk.pred
  stk.full <- inla.stack(stk.fit, stk.pred)

  # build the model - this overwrites the model loaded from the Rda file? (best model found through Script_find_best_model.R?)
  model <-inla(formula,
                data=inla.stack.data(stk.full,spde=spde), # data plus spde
                family= 'binomial', # data family
                control.family = list(link = "logit"), # logit for logistic regression
                control.predictor=list(link = 1, A=inla.stack.A(stk.full)), # calculate the covariate weights
                control.compute = list(waic = TRUE) # calculate Watanabe-Akaike information criterion 
                )

  index_pred          <- inla.stack.index(stack = stk.full, tag = "pred")$data
  predictions_pred    <- model$summary.fitted.values[index_pred, "mean"]
#~   predictions_pred[is.na(predictions_pred)] <- 0.5
  pseudoRsquared_pred <- 1 - (sum((ground_truth - predictions_pred)^2)/sum((ground_truth - rep(mean_ground_truth))^2))
  PI_grid_log         <- 1 - cor(predictions,predictions_pred,method='spearman')
  print(paste('pR2:',pseudoRsquared,'pR2 random grid:',pseudoRsquared_pred,'PI grid:',PI_grid_log))

  sign                <- if(sign(tab_coef['grid_log',]) < 0) '#cc3300' else '#3399FF'	
  df_PI_grid          <- data.frame('Grid distance',PI_grid_log,sign)
  names(df_PI_grid)   <- c('predictor','PI','sign')
} else {
  df_PI_grid          <- data.frame('Grid distance',0,0)
  names(df_PI_grid)   <- c('predictor','PI','sign')	
}

#----------------------------------------------------------------------------------------------------
# importance of protect - create stack with randomized values
if (tab_coef['protect',] != 0){
  stk.pred <- inla.stack(data=list(y = rep(NA, length(testFrame[,1])), n = length(testFrame[,1])),
                            effects=list(c(list(Intercept=1), iset),
                                           list(SOVEREIGN1 = testFrame$SOVEREIGN1,
                                                travel_log = testFrame$travel_log,
                                                slope_log = testFrame$slope_log,
                                                elev_log = testFrame$elev_log,
                                                grid_log = testFrame$grid_log,
                                                road_log = testFrame$road_log, 
                                                protect = sample(testFrame$protect,replace=FALSE),
                                                rsds = testFrame$rsds,
                                                Agriculture = testFrame$Agriculture,
                                                Short_natural = testFrame$Short_natural,
                                                Wetland = testFrame$Wetland,
                                                Urban = testFrame$Urban,
                                                Bare = testFrame$Bare,
                                                Water = testFrame$Water
                                        )
                                    ), 
                            A=list(A,1),
                            tag='pred')

  # stk.full has stk.fit and stk.pred
  stk.full <- inla.stack(stk.fit, stk.pred)

  # build the model - this overwrites the model loaded from the Rda file? (best model found through Script_find_best_model.R?)
  model <-inla(formula,
                data=inla.stack.data(stk.full,spde=spde), # data plus spde
                family= 'binomial', # data family
                control.family = list(link = "logit"), # logit for logistic regression
                control.predictor=list(link = 1, A=inla.stack.A(stk.full)), # calculate the covariate weights
                control.compute = list(waic = TRUE) # calculate Watanabe-Akaike information criterion 
                )

  index_pred          <- inla.stack.index(stack = stk.full, tag = "pred")$data
  predictions_pred    <- model$summary.fitted.values[index_pred, "mean"]
#~   predictions_pred[is.na(predictions_pred)] <- 0.5
  pseudoRsquared_pred <- 1 - (sum((ground_truth - predictions_pred)^2)/sum((ground_truth - rep(mean_ground_truth))^2))
  PI_protect          <- 1 - cor(predictions,predictions_pred,method='spearman')
  print(paste('pR2:',pseudoRsquared,'pR2 random protect:',pseudoRsquared_pred,'PI protect:',PI_protect))

  sign                <- if(sign(tab_coef['protect',]) < 0) '#cc3300' else '#3399FF'	
  df_PI_protect       <- data.frame('Protected status',PI_grid_log,sign)
  names(df_PI_protect)<- c('predictor','PI','sign')
} else {
  df_PI_protect       <- data.frame('Protected status',0,0)
  names(df_PI_protect)<- c('predictor','PI','sign')	
}

#-------------------------------------------------------------------------------------------------------
# importance of rsds - create stack with randomized values
if (tab_coef['rsds',] != 0){
  stk.pred <- inla.stack(data=list(y = rep(NA, length(testFrame[,1])), n = length(testFrame[,1])),
                            effects=list(c(list(Intercept=1), iset),
                                           list(SOVEREIGN1 = testFrame$SOVEREIGN1,
                                                travel_log = testFrame$travel_log,
                                                slope_log = testFrame$slope_log,
                                                elev_log = testFrame$elev_log,
                                                grid_log = testFrame$grid_log,
                                                road_log = testFrame$road_log, 
                                                protect = testFrame$protect,
                                                rsds = sample(testFrame$rsds,replace=FALSE),
                                                Agriculture = testFrame$Agriculture,
                                                Short_natural = testFrame$Short_natural,
                                                Wetland = testFrame$Wetland,
                                                Urban = testFrame$Urban,
                                                Bare = testFrame$Bare,
                                                Water = testFrame$Water
                                        )
                                    ), 
                            A=list(A,1),
                            tag='pred')

  # stk.full has stk.fit and stk.pred
  stk.full <- inla.stack(stk.fit, stk.pred)

  # build the model - this overwrites the model loaded from the Rda file? (best model found through Script_find_best_model.R?)
  model <-inla(formula,
                data=inla.stack.data(stk.full,spde=spde), # data plus spde
                family= 'binomial', # data family
                control.family = list(link = "logit"), # logit for logistic regression
                control.predictor=list(link = 1, A=inla.stack.A(stk.full)), # calculate the covariate weights
                control.compute = list(waic = TRUE) # calculate Watanabe-Akaike information criterion 
                )

  index_pred          <- inla.stack.index(stack = stk.full, tag = "pred")$data
  predictions_pred    <- model$summary.fitted.values[index_pred, "mean"]
#~   predictions_pred[is.na(predictions_pred)] <- 0.5
  pseudoRsquared_pred <- 1 - (sum((ground_truth - predictions_pred)^2)/sum((ground_truth - rep(mean_ground_truth))^2))
  PI_rsds             <- 1 - cor(predictions,predictions_pred,method='spearman')
  print(paste('pR2:',pseudoRsquared,'pR2 random rsds:',pseudoRsquared_pred,'PI rsds:',PI_rsds))

  sign                <- if(sign(tab_coef['rsds',]) < 0) '#cc3300' else '#3399FF'	
  df_PI_rsds          <- data.frame('Irradiation',PI_rsds,sign)
  names(df_PI_rsds)   <- c('predictor','PI','sign')
} else {
  df_PI_rsds          <- data.frame('Irradiation',0,0)
  names(df_PI_rsds)   <- c('predictor','PI','sign')	
}

#-------------------------------------------------------------------------------------------------------
# importance of Agriculture - create stack with randomized values
if (tab_coef['Agriculture',] != 0){
  stk.pred <- inla.stack(data=list(y = rep(NA, length(testFrame[,1])), n = length(testFrame[,1])),
                            effects=list(c(list(Intercept=1), iset),
                                           list(SOVEREIGN1 = testFrame$SOVEREIGN1,
                                                travel_log = testFrame$travel_log,
                                                slope_log = testFrame$slope_log,
                                                elev_log = testFrame$elev_log,
                                                grid_log = testFrame$grid_log,
                                                road_log = testFrame$road_log, 
                                                protect = testFrame$protect,
                                                rsds = testFrame$rsds,
                                                Agriculture = sample(testFrame$Agriculture,replace=FALSE),
                                                Short_natural = testFrame$Short_natural,
                                                Wetland = testFrame$Wetland,
                                                Urban = testFrame$Urban,
                                                Bare = testFrame$Bare,
                                                Water = testFrame$Water
                                        )
                                    ), 
                            A=list(A,1),
                            tag='pred')

  # stk.full has stk.fit and stk.pred
  stk.full <- inla.stack(stk.fit, stk.pred)

  # build the model - this overwrites the model loaded from the Rda file? (best model found through Script_find_best_model.R?)
  model <-inla(formula,
                data=inla.stack.data(stk.full,spde=spde), # data plus spde
                family= 'binomial', # data family
                control.family = list(link = "logit"), # logit for logistic regression
                control.predictor=list(link = 1, A=inla.stack.A(stk.full)), # calculate the covariate weights
                control.compute = list(waic = TRUE) # calculate Watanabe-Akaike information criterion 
                )

  index_pred          <- inla.stack.index(stack = stk.full, tag = "pred")$data
  predictions_pred    <- model$summary.fitted.values[index_pred, "mean"]
#~   predictions_pred[is.na(predictions_pred)] <- 0.5
  pseudoRsquared_pred <- 1 - (sum((ground_truth - predictions_pred)^2)/sum((ground_truth - rep(mean_ground_truth))^2))
  PI_Agriculture      <- 1 - cor(predictions,predictions_pred,method='spearman')
  print(paste('pR2:',pseudoRsquared,'pR2 random Agriculture:',pseudoRsquared_pred,'PI Agriculture:',PI_Agriculture))

  sign                <- if(sign(tab_coef['Agriculture',]) < 0) '#cc3300' else '#3399FF'	
  df_PI_Agriculture   <- data.frame('Agriculture',PI_Agriculture,sign)
  names(df_PI_Agriculture) <- c('predictor','PI','sign')
} else {
  df_PI_Agriculture   <- data.frame('Agriculture',0,0)
  names(df_PI_Agriculture) <- c('predictor','PI','sign')	
}

#-------------------------------------------------------------------------------------------------------
# importance of Short_natural - create stack with randomized values
if (tab_coef['Short_natural',] != 0){
  stk.pred <- inla.stack(data=list(y = rep(NA, length(testFrame[,1])), n = length(testFrame[,1])),
                            effects=list(c(list(Intercept=1), iset),
                                           list(SOVEREIGN1 = testFrame$SOVEREIGN1,
                                                travel_log = testFrame$travel_log,
                                                slope_log = testFrame$slope_log,
                                                elev_log = testFrame$elev_log,
                                                grid_log = testFrame$grid_log,
                                                road_log = testFrame$road_log, 
                                                protect = testFrame$protect,
                                                rsds = testFrame$rsds,
                                                Agriculture = testFrame$Agriculture,
                                                Short_natural = sample(testFrame$Short_natural,replace=FALSE),
                                                Wetland = testFrame$Wetland,
                                                Urban = testFrame$Urban,
                                                Bare = testFrame$Bare,
                                                Water = testFrame$Water
                                        )
                                    ), 
                            A=list(A,1),
                            tag='pred')

  # stk.full has stk.fit and stk.pred
  stk.full <- inla.stack(stk.fit, stk.pred)

  # build the model - this overwrites the model loaded from the Rda file? (best model found through Script_find_best_model.R?)
  model <-inla(formula,
                data=inla.stack.data(stk.full,spde=spde), # data plus spde
                family= 'binomial', # data family
                control.family = list(link = "logit"), # logit for logistic regression
                control.predictor=list(link = 1, A=inla.stack.A(stk.full)), # calculate the covariate weights
                control.compute = list(waic = TRUE) # calculate Watanabe-Akaike information criterion 
                )

  index_pred          <- inla.stack.index(stack = stk.full, tag = "pred")$data
  predictions_pred    <- model$summary.fitted.values[index_pred, "mean"]
#~   predictions_pred[is.na(predictions_pred)] <- 0.5
  pseudoRsquared_pred <- 1 - (sum((ground_truth - predictions_pred)^2)/sum((ground_truth - rep(mean_ground_truth))^2))
  PI_Short_natural    <- 1 - cor(predictions,predictions_pred,method='spearman')
  print(paste('pR2:',pseudoRsquared,'pR2 random Short_natural:',pseudoRsquared_pred,'PI Short_natural:',PI_Short_natural))

  sign                <- if(sign(tab_coef['Short_natural',]) < 0) '#cc3300' else '#3399FF'	
  df_PI_Short_natural <- data.frame('Short natural',PI_Short_natural,sign)
  names(df_PI_Short_natural) <- c('predictor','PI','sign')
} else {
  df_PI_Short_natural <- data.frame('Short natural',0,0)
  names(df_PI_Short_natural) <- c('predictor','PI','sign')	
}

#-------------------------------------------------------------------------------------------------------
# importance of Wetland - create stack with randomized values
if (tab_coef['Wetland',] != 0){
  stk.pred <- inla.stack(data=list(y = rep(NA, length(testFrame[,1])), n = length(testFrame[,1])),
                            effects=list(c(list(Intercept=1), iset),
                                           list(SOVEREIGN1 = testFrame$SOVEREIGN1,
                                                travel_log = testFrame$travel_log,
                                                slope_log = testFrame$slope_log,
                                                elev_log = testFrame$elev_log,
                                                grid_log = testFrame$grid_log,
                                                road_log = testFrame$road_log, 
                                                protect = testFrame$protect,
                                                rsds = testFrame$rsds,
                                                Agriculture = testFrame$Agriculture,
                                                Short_natural = testFrame$Short_natural,
                                                Wetland = sample(testFrame$Wetland,replace=FALSE),
                                                Urban = testFrame$Urban,
                                                Bare = testFrame$Bare,
                                                Water = testFrame$Water
                                        )
                                    ), 
                            A=list(A,1),
                            tag='pred')

  # stk.full has stk.fit and stk.pred
  stk.full <- inla.stack(stk.fit, stk.pred)

  # build the model - this overwrites the model loaded from the Rda file? (best model found through Script_find_best_model.R?)
  model <-inla(formula,
                data=inla.stack.data(stk.full,spde=spde), # data plus spde
                family= 'binomial', # data family
                control.family = list(link = "logit"), # logit for logistic regression
                control.predictor=list(link = 1, A=inla.stack.A(stk.full)), # calculate the covariate weights
                control.compute = list(waic = TRUE) # calculate Watanabe-Akaike information criterion 
                )

  index_pred          <- inla.stack.index(stack = stk.full, tag = "pred")$data
  predictions_pred    <- model$summary.fitted.values[index_pred, "mean"]
#~   predictions_pred[is.na(predictions_pred)] <- 0.5
  pseudoRsquared_pred <- 1 - (sum((ground_truth - predictions_pred)^2)/sum((ground_truth - rep(mean_ground_truth))^2))
  PI_Wetland          <- 1 - cor(predictions,predictions_pred,method='spearman')
  print(paste('pR2:',pseudoRsquared,'pR2 random Wetland:',pseudoRsquared_pred,'PI Wetland:',PI_Wetland))

  sign                <- if(sign(tab_coef['Wetland',]) < 0) '#cc3300' else '#3399FF'	
  df_PI_Wetland       <- data.frame('Wetland',PI_Wetland,sign)
  names(df_PI_Wetland)<- c('predictor','PI','sign')
} else {
  df_PI_Wetland       <- data.frame('Wetland',0,0)
  names(df_PI_Wetland)<- c('predictor','PI','sign')	
}

#-------------------------------------------------------------------------------------------------------
# importance of Urban - create stack with randomized values
if (tab_coef['Urban',] != 0){
  stk.pred <- inla.stack(data=list(y = rep(NA, length(testFrame[,1])), n = length(testFrame[,1])),
                            effects=list(c(list(Intercept=1), iset),
                                           list(SOVEREIGN1 = testFrame$SOVEREIGN1,
                                                travel_log = testFrame$travel_log,
                                                slope_log = testFrame$slope_log,
                                                elev_log = testFrame$elev_log,
                                                grid_log = testFrame$grid_log,
                                                road_log = testFrame$road_log, 
                                                protect = testFrame$protect,
                                                rsds = testFrame$rsds,
                                                Agriculture = testFrame$Agriculture,
                                                Short_natural = testFrame$Short_natural,
                                                Wetland = testFrame$Wetland,
                                                Urban = sample(testFrame$Urban,replace=FALSE),
                                                Bare = testFrame$Bare,
                                                Water = testFrame$Water
                                        )
                                    ), 
                            A=list(A,1),
                            tag='pred')

  # stk.full has stk.fit and stk.pred
  stk.full <- inla.stack(stk.fit, stk.pred)

  # build the model - this overwrites the model loaded from the Rda file? (best model found through Script_find_best_model.R?)
  model <-inla(formula,
                data=inla.stack.data(stk.full,spde=spde), # data plus spde
                family= 'binomial', # data family
                control.family = list(link = "logit"), # logit for logistic regression
                control.predictor=list(link = 1, A=inla.stack.A(stk.full)), # calculate the covariate weights
                control.compute = list(waic = TRUE) # calculate Watanabe-Akaike information criterion 
                )

  index_pred          <- inla.stack.index(stack = stk.full, tag = "pred")$data
  predictions_pred    <- model$summary.fitted.values[index_pred, "mean"]
#~   predictions_pred[is.na(predictions_pred)] <- 0.5
  pseudoRsquared_pred <- 1 - (sum((ground_truth - predictions_pred)^2)/sum((ground_truth - rep(mean_ground_truth))^2))
  PI_Urban            <- 1 - cor(predictions,predictions_pred,method='spearman')
  print(paste('pR2:',pseudoRsquared,'pR2 random Urban:',pseudoRsquared_pred,'PI Urban:',PI_Urban))

  sign                <- if(sign(tab_coef['Urban',]) < 0) '#cc3300' else '#3399FF'	
  df_PI_Urban         <- data.frame('Urban',PI_Urban,sign)
  names(df_PI_Urban)  <- c('predictor','PI','sign')
} else {
  df_PI_Urban         <- data.frame('Urban',0,0)
  names(df_PI_Urban)  <- c('predictor','PI','sign')	
}

#-------------------------------------------------------------------------------------------------------
# importance of Bare - create stack with randomized values
if (tab_coef['Bare',] != 0){
  stk.pred <- inla.stack(data=list(y = rep(NA, length(testFrame[,1])), n = length(testFrame[,1])),
                            effects=list(c(list(Intercept=1), iset),
                                           list(SOVEREIGN1 = testFrame$SOVEREIGN1,
                                                travel_log = testFrame$travel_log,
                                                slope_log = testFrame$slope_log,
                                                elev_log = testFrame$elev_log,
                                                grid_log = testFrame$grid_log,
                                                road_log = testFrame$road_log, 
                                                protect = testFrame$protect,
                                                rsds = testFrame$rsds,
                                                Agriculture = testFrame$Agriculture,
                                                Short_natural = testFrame$Short_natural,
                                                Wetland = testFrame$Wetland,
                                                Urban = testFrame$Urban,
                                                Bare = sample(testFrame$Bare,replace=FALSE),
                                                Water = testFrame$Water
                                        )
                                    ), 
                            A=list(A,1),
                            tag='pred')

  # stk.full has stk.fit and stk.pred
  stk.full <- inla.stack(stk.fit, stk.pred)

  # build the model - this overwrites the model loaded from the Rda file? (best model found through Script_find_best_model.R?)
  model <-inla(formula,
                data=inla.stack.data(stk.full,spde=spde), # data plus spde
                family= 'binomial', # data family
                control.family = list(link = "logit"), # logit for logistic regression
                control.predictor=list(link = 1, A=inla.stack.A(stk.full)), # calculate the covariate weights
                control.compute = list(waic = TRUE) # calculate Watanabe-Akaike information criterion 
                )

  index_pred          <- inla.stack.index(stack = stk.full, tag = "pred")$data
  predictions_pred    <- model$summary.fitted.values[index_pred, "mean"]
#~   predictions_pred[is.na(predictions_pred)] <- 0.5
  pseudoRsquared_pred <- 1 - (sum((ground_truth - predictions_pred)^2)/sum((ground_truth - rep(mean_ground_truth))^2))
  PI_Bare             <- 1 - cor(predictions,predictions_pred,method='spearman')
  print(paste('pR2:',pseudoRsquared,'pR2 random Bare:',pseudoRsquared_pred,'PI Bare:',PI_Bare))

  sign                <- if(sign(tab_coef['Bare',]) < 0) '#cc3300' else '#3399FF'	
  df_PI_Bare          <- data.frame('Bare',PI_Bare,sign)
  names(df_PI_Bare)   <- c('predictor','PI','sign')
} else {
  df_PI_Bare          <- data.frame('Bare',0,0)
  names(df_PI_Bare)   <- c('predictor','PI','sign')	
}

#-------------------------------------------------------------------------------------------------------
# importance of Water - create stack with randomized values
if (tab_coef['Water',] != 0){
  stk.pred <- inla.stack(data=list(y = rep(NA, length(testFrame[,1])), n = length(testFrame[,1])),
                            effects=list(c(list(Intercept=1), iset),
                                           list(SOVEREIGN1 = testFrame$SOVEREIGN1,
                                                travel_log = testFrame$travel_log,
                                                slope_log = testFrame$slope_log,
                                                elev_log = testFrame$elev_log,
                                                grid_log = testFrame$grid_log,
                                                road_log = testFrame$road_log, 
                                                protect = testFrame$protect,
                                                rsds = testFrame$rsds,
                                                Agriculture = testFrame$Agriculture,
                                                Short_natural = testFrame$Short_natural,
                                                Wetland = testFrame$Wetland,
                                                Urban = testFrame$Urban,
                                                Bare = testFrame$Bare,
                                                Water = sample(testFrame$Water,replace=FALSE)
                                        )
                                    ), 
                            A=list(A,1),
                            tag='pred')

  # stk.full has stk.fit and stk.pred
  stk.full <- inla.stack(stk.fit, stk.pred)

  # build the model - this overwrites the model loaded from the Rda file? (best model found through Script_find_best_model.R?)
  model <-inla(formula,
                data=inla.stack.data(stk.full,spde=spde), # data plus spde
                family= 'binomial', # data family
                control.family = list(link = "logit"), # logit for logistic regression
                control.predictor=list(link = 1, A=inla.stack.A(stk.full)), # calculate the covariate weights
                control.compute = list(waic = TRUE) # calculate Watanabe-Akaike information criterion 
                )

  index_pred          <- inla.stack.index(stack = stk.full, tag = "pred")$data
  predictions_pred    <- model$summary.fitted.values[index_pred, "mean"]
#~   predictions_pred[is.na(predictions_pred)] <- 0.5
  pseudoRsquared_pred <- 1 - (sum((ground_truth - predictions_pred)^2)/sum((ground_truth - rep(mean_ground_truth))^2))
  PI_Water            <- 1 - cor(predictions,predictions_pred,method='spearman')
  print(paste('pR2:',pseudoRsquared,'pR2 random Water:',pseudoRsquared_pred,'PI Water:',PI_Water))

  sign                <- if(sign(tab_coef['Water',]) < 0) '#cc3300' else '#3399FF'	
  df_PI_Water         <- data.frame('Water',PI_Water,sign)
  names(df_PI_Water)  <- c('predictor','PI','sign')
} else {
  df_PI_Water         <- data.frame('Water',0,0)
  names(df_PI_Water)  <- c('predictor','PI','sign')	
}

#-------------------------------------------------------------------------------------------------------
# importance of country - create stack with randomized values
stk.pred <- inla.stack(data=list(y = rep(NA, length(testFrame[,1])), n = length(testFrame[,1])),
                            effects=list(c(list(Intercept=1), iset),
                                           list(SOVEREIGN1 = sample(testFrame$SOVEREIGN1,replace=FALSE),
                                                travel_log = testFrame$travel_log,
                                                slope_log = testFrame$slope_log,
                                                elev_log = testFrame$elev_log,
                                                grid_log = testFrame$grid_log,
                                                road_log = testFrame$road_log, 
                                                protect = testFrame$protect,
                                                rsds = testFrame$rsds,
                                                Agriculture = testFrame$Agriculture,
                                                Short_natural = testFrame$Short_natural,
                                                Wetland = testFrame$Wetland,
                                                Urban = testFrame$Urban,
                                                Bare = testFrame$Bare,
                                                Water = testFrame$Water
                                        )
                                    ), 
                            A=list(A,1),
                            tag='pred')

# stk.full has stk.fit and stk.pred
stk.full <- inla.stack(stk.fit, stk.pred)

# build the model - this overwrites the model loaded from the Rda file? (best model found through Script_find_best_model.R?)
model <-inla(formula,
                data=inla.stack.data(stk.full,spde=spde), # data plus spde
                family= 'binomial', # data family
                control.family = list(link = "logit"), # logit for logistic regression
                control.predictor=list(link = 1, A=inla.stack.A(stk.full)), # calculate the covariate weights
                control.compute = list(waic = TRUE) # calculate Watanabe-Akaike information criterion 
                )

index_pred          <- inla.stack.index(stack = stk.full, tag = "pred")$data
predictions_pred    <- model$summary.fitted.values[index_pred, "mean"]
#~ predictions_pred[is.na(predictions_pred)] <- 0.5
pseudoRsquared_pred <- 1 - (sum((ground_truth - predictions_pred)^2)/sum((ground_truth - rep(mean_ground_truth))^2))
PI_country          <- 1 - cor(predictions,predictions_pred,method='spearman')
print(paste('pR2:',pseudoRsquared,'pR2 random country:',pseudoRsquared_pred,'PI country:',PI_country))

sign                <- 0 	# country is not negatively or positively correlated
df_PI_country       <- data.frame('Country',PI_country,sign)
names(df_PI_country)<- c('predictor','PI','sign')

df_PI <- rbind(df_PI_country,df_PI_Wetland,df_PI_Water,df_PI_Urban,df_PI_Short_natural,df_PI_Bare,df_PI_Agriculture,df_PI_travel,df_PI_slope,df_PI_road,df_PI_protect,df_PI_rsds,df_PI_grid,df_PI_elev)
df_PI
write_xlsx(df_PI,'df_PI_Afr.xlsx')
