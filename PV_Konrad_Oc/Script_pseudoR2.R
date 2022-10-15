pacman::p_load(ggplot2, writexl,data.table,readxl,INLA,pROC)

best_model <- '3998'
dir_Konrad <- '/vol/milkunarc2/kmielke/pv' 

# load df_PA (data frame), in which predictors are already log-transformed and standardized
load(file = file.path(dir_Konrad,'/data/rda/df_PA_Oc_country_proprocessed.Rda'))
df_PA_orig <- read_xlsx('../PV_determinants_Oc/df_PA_Oc_country.xlsx')

# load model (copied to this dir in Script_find_best_model)
load(paste0(best_model,'.Rda'))

##### compute pseudoR2 - first some general modelling
model_performance <- read.csv('waic_OcRev.txt', sep = ' ', header = FALSE)
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
                                                #slope_log = trainFrame$slope_log,
                                                elev_log = trainFrame$elev_log,
                                                road_log = trainFrame$road_log,
                                                grid_log = trainFrame$grid_log,
                                                Wetland = trainFrame$Wetland,
                                                Urban = trainFrame$Urban
                                        )
                                    ), 
                            A=list(A,1),
                            tag='fit')

# create stack of data used for prediction (this should be the one with randomized values etc. stk.fit should always be the original data frame)                          
stk.pred <- inla.stack(data=list(y = as.integer(testFrame$PA_bool, n = length(testFrame$PA_bool))),
                            effects=list(c(list(Intercept=1), iset),
                                           list(SOVEREIGN1 = rep(NA, length(testFrame$SOVEREIGN1)),
                                                travel_log = testFrame$travel_log,
                                                #slope_log = testFrame$slope_log,
                                                elev_log = testFrame$elev_log,
                                                road_log = testFrame$road_log,
                                                grid_log = testFrame$grid_log,
                                                Wetland = testFrame$Wetland,
                                                Urban = testFrame$Urban
                                        )
                                    ), 
                            A=list(A,1),
                            tag='pred')


# stk.full has stk.fit and stk.pred
stk.full <- inla.stack(stk.fit, stk.pred)

formula <- y ~ 1 + travel_log + road_log + elev_log + grid_log + Wetland + Urban + f(SOVEREIGN1, model = "iid") + f(spatial.field, model = spde)

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

##### pseudo R square calculation (for R square the ground truth has been know, so I'm using 'fit' dataset here

# get the predictions for training data
index_fit  <- inla.stack.index(stack = stk.full, tag = "fit")$data
index_pred <- inla.stack.index(stack = stk.full, tag = "pred")$data
predictions_fit  <- model$summary.fitted.values[index_fit, "mean"]
predictions_pred <- model$summary.fitted.values[index_pred, "mean"]
ground_truth <- as.integer(trainFrame$PA_bool)
mean_ground_truth <- mean(ground_truth)

pseudoRsquared_fit <- 1 - (sum((ground_truth - predictions_fit)^2)/sum((ground_truth - rep(mean_ground_truth))^2))
pseudoRsquared_fit	# 'original' predictions
pseudoRsquared_pred <- 1 - (sum((ground_truth - predictions_pred)^2)/sum((ground_truth - rep(mean_ground_truth))^2))
pseudoRsquared_pred # predictions without country

# other model statistics
theROC <- roc(ground_truth, predictions_fit)
auc    <- theROC$auc
tpr    <- coords(theROC, "best", ret = c("sensitivity"))
tnr    <- coords(theROC, "best", ret = c("specificity"))
tss    <- coords(theROC, "best", ret = c("sensitivity")) + coords(theROC,"best", ret = c("specificity")) - 1

# create model statistic dataframe
cols <- c('pR2','pR2_nocountry','AUC','TPR','TNR','TSS')
data <- c(pseudoRsquared_fit,pseudoRsquared_pred,auc[1],tpr[1],tnr[1],tss[1])

df_scores <- data.frame(data)
colnames(df_scores) <- cols
write_xlsx(df_scores,'df_scores_NAm.xlsx')

# other model statistics WITHOUT COUNTRY
theROC <- roc(ground_truth, predictions_pred)
auc    <- theROC$auc
tpr    <- coords(theROC, "best", ret = c("sensitivity"))
tnr    <- coords(theROC, "best", ret = c("specificity"))
tss    <- coords(theROC, "best", ret = c("sensitivity")) + coords(theROC,"best", ret = c("specificity")) - 1

# create model statistic dataframe
cols <- c('pR2','pR2_nocountry','AUC','TPR','TNR','TSS')
data <- c(pseudoRsquared_fit,pseudoRsquared_pred,auc[1],tpr[1],tnr[1],tss[1])

df_scores_nocountry <- data.frame(data)
colnames(df_scores_nocountry) <- cols
df_scores_nocountry <- rbind(df_scores_nocountry,c('-','-',(df_scores_nocountry$AUC-df_scores$AUC)*100/df_scores$AUC,(df_scores_nocountry$TPR-df_scores$TPR)*100/df_scores$TPR,(df_scores_nocountry$TNR-df_scores$TNR)*100/df_scores$TNR,(df_scores_nocountry$TSS-df_scores$TSS)*100/df_scores$TSS))
write_xlsx(df_scores_nocountry,'df_scores_nocountry_NAm.xlsx')
