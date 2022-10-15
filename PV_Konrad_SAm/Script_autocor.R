pacman::p_load(readxl,ggplot2,RColorBrewer,ape,INLA,geosphere)

best_model <- '1138'
dir_Konrad <- '/vol/milkunarc2/kmielke/pv' 

# load df_PA (data frame), in which predictors are already log-transformed and standardized
load(file = file.path(dir_Konrad,'/data/rda/df_PA_SAm_country_060622_proprocessed.Rda'))
df_PA_orig <- read_xlsx('../PV_determinants_SAm/df_PA_SAm_country_060622.xlsx')

# load model (copied to this dir in Script_find_best_model)
load(paste0(best_model,'.Rda'))

##### manual residual calculation - need some general modelling as well 
model_performance <- read.csv(file.path(dir_Konrad,'/results/waic_SAmRev.txt'), sep = ' ', header = FALSE)
print(paste('check that best model is ',best_model,':',model_performance[which.min(model_performance$V2), 1]))

# create matrix of coordinate pairs
coords = cbind(df_PA$x_coord, df_PA$y_coord)
PA <- nrow(df_PA)

# calculate range of x and y directions
x_range = max(coords[,1]) - min(coords[,1])
y_range = max(coords[,2]) - min(coords[,2])

# average ranges
mean_range = (x_range + y_range)/2

# create mesh
# the mesh is the spatial system for which the spatial random effects will be calculated.
# cutoff is the minimum distance allowed between points of the mesh
# max.edge is the maximum distance allowed
# smaller values lead to higher computation time, but potentially a more detailed model
# guidelines for parameters: max.edge should be between 1/3 and 1/5 of range, cutoff should be 1/5 of max.edge
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
#stk.fit <- inla.stack(data=list(y = as.integer(trainFrame[,1], n = length(trainFrame[,1]))),
stk.fit <- inla.stack(data=list(y = as.integer(trainFrame$PA_bool, n = length(trainFrame$PA_bool))),
                            effects=list(c(list(Intercept=1), iset),
                                           list(SOVEREIGN1 = trainFrame$SOVEREIGN1,
                                                travel_log = trainFrame$travel_log,
                                                slope_log = trainFrame$slope_log,
                                                #elev_log = trainFrame$elev_log,
                                                road_log = trainFrame$road_log,
                                                grid_log = trainFrame$grid_log,
                                                #protect = trainFrame$protect,
                                                rsds = trainFrame$rsds
                                                #Agriculture = trainFrame$Agriculture,
                                                #Short_natural = trainFrame$Short_natural,
                                                #Wetland = trainFrame$Wetland,
                                                #Urban = trainFrame$Urban
                                        )
                                    ), 
                            A=list(A,1),
                            tag='fit')

# create stack of data used for prediction (this should be the one with randomized values etc. stk.fit should always be the original data frame)                          
stk.pred <- inla.stack(data=list(y = rep(NA, length(testFrame[,1])), n = length(testFrame[,1])),
                            effects=list(c(list(Intercept=1), iset),
                                           list(SOVEREIGN1 = testFrame$SOVEREIGN1,
                                                travel_log = testFrame$travel_log,
                                                slope_log = testFrame$slope_log,
                                                #elev_log = testFrame$elev_log,
                                                road_log = testFrame$road_log,
                                                grid_log = testFrame$grid_log,
                                                #protect = testFrame$protect,
                                                rsds = testFrame$rsds
                                                #Agriculture = testFrame$Agriculture,
                                                #Short_natural = testFrame$Short_natural,
                                                #Wetland = testFrame$Wetland,
                                                #Urban = testFrame$Urban
                                        )
                                    ), 
                            A=list(A,1),
                            tag='pred')

# stk.full has stk.fit and stk.pred
stk.full <- inla.stack(stk.fit, stk.pred)

# get predictions
index <- inla.stack.index(stack = stk.full, tag = "fit")$data
p <- model$summary.fitted.values[index, "mean"]
q <- 1-p
r <- as.integer(trainFrame$PA_bool)

# calculate residuals
residuals_inla <- ifelse(r == 1, sqrt(-2*log(p)), - sqrt(-2*log(q)))

df_PA$res <- residuals_inla    #obs minus fitted

# check distribution of residuals
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(-2.5,2.5))

pdf(file='Plot_residuals.pdf')
ggplot() + theme_minimal() + geom_point(data=df_PA,aes(x=x_coord,y=y_coord,color=res),size=0.5) + sc
dev.off()

# compute Moran's I
#distances <- distm(df_PA_orig$lon_orig, df_PA$lat_orig,distGeo)	#original lons and lats
distances <- distm(cbind(df_PA_orig$lon_orig,df_PA_orig$lat_orig),fun=distGeo)
weights   <- 1/distances
weights[is.infinite(weights)] <- 0

moranI    <- Moran.I(df_PA$res,weights)		
moranI


save(moranI,file='moranI_SAm.Rda')

#~ # also test with distances from Jelle's computations (PA to PA)
#~ dir_Jelle   <- '/vol/milkunA/jhilbers/Main_folders/Jelle/Code_advies/Joyce_2022'
#~ file_Jelle  <- 'distance_matrix_SAm.csv'
#~ dist_matrix <- read.csv(file.path(dir_Jelle,file_Jelle))

#~ distances <- matrix(dist_matrix$dist,nrow=PA)
#~ weights   <- 1/distances
#~ weights[is.infinite(weights)] <- 0

#~ moranI    <- Moran.I(df_PA$res,weights)		
#~ moranI

#~ # same results so distance computations are OK
