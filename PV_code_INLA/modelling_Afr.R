library(foreach)
library(doParallel)

registerDoParallel(25)

# load packages
pacman::p_load(readxl,ggplot2,RColorBrewer,ape,INLA,geosphere)

# main directory
dir_Konrad <- '/vol/milkunarc2/kmielke/pv' 

# load df_PA (data frame), in which predictors are already log-transformed and standardized
load(file = file.path(dir_Konrad, '/data/rda/df_PA_Afr_country_proprocessed.Rda'))

# load combs (combinations of variables)
load(file = file.path(dir_Konrad, '/data/rda/var_combs/combinations_Afr.Rda'))

# create matrix of coordinate pairs
coords = cbind(df_PA$x_coord, df_PA$y_coord)

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
A<-inla.spde.make.A(mesh=mesh,loc=as.matrix(coords))

# spde is the spatial structure, here I chose a matern structure with alpha = 1.5 which is an exponential decay
spde <- inla.spde2.matern(mesh, alpha=1.5)

# another helping structure, this gives an index to each position of the mesh
iset <- inla.spde.make.index(name = "spatial.field", spde$n.spde)

# create random seeds
seeds <- sample(-2000000000:2000000000, nrow(combs))

# loop over all variable combinations
foreach(i = 1:nrow(combs)) %dopar% {

    # take one of created seeds
    set.seed(seeds[i])
    
    # get variables and their count
    vars <- combs[i, !is.na(combs[i, ])]    
    n_vars <- length(vars)

    # create stack and formula, depending on number of variables in the model
     if (n_vars == 1){
        stk <- inla.stack(data=list(y = as.integer(unlist(df_PA[,1]))),
                            effects=list(c(list(Intercept=1), iset),
                                           list(VC = unlist(df_PA[,2]), V1 = unlist(df_PA[,vars[[1]]])
                                        )
                                    ), 
                            A=list(A,1),
                            tag='fit')
        formula <- y ~ 1 + V1 + f(VC, model = "iid") + f(spatial.field, model = spde)

    }

    if (n_vars == 2){
        stk <- inla.stack(data=list(y = as.integer(unlist(df_PA[,1]))),
                            effects=list(c(list(Intercept=1), iset),
                                           list(VC = unlist(df_PA[,2]), V1 = unlist(df_PA[,vars[[1]]]), V2 = unlist(df_PA[,vars[[2]]])
                                        )
                                    ), 
                            A=list(A,1),
                            tag='fit')
        formula <- y ~ 1 + V1 + V2 + f(VC, model = "iid") + f(spatial.field, model = spde)

    }

     if (n_vars == 3){
        stk <- inla.stack(data=list(y = as.integer(unlist(df_PA[,1]))),
                            effects=list(c(list(Intercept=1), iset),
                                           list(VC = unlist(df_PA[,2]), V1 = unlist(df_PA[,vars[[1]]]), V2 = unlist(df_PA[,vars[[2]]]), V3 = unlist(df_PA[,vars[[3]]])
                                        )
                                    ), 
                            A=list(A,1),
                            tag='fit')
        formula <- y ~ 1 + V1 + V2 + V3 + f(VC, model = "iid") + f(spatial.field, model = spde)

    }

    if (n_vars == 4){
        stk <- inla.stack(data=list(y = as.integer(unlist(df_PA[,1]))),
                            effects=list(c(list(Intercept=1), iset),
                                           list(VC = unlist(df_PA[,2]), V1 = unlist(df_PA[,vars[[1]]]), V2 = unlist(df_PA[,vars[[2]]]), V3 = unlist(df_PA[,vars[[3]]]), V4 = unlist(df_PA[,vars[[4]]])
                                        )
                                    ), 
                            A=list(A,1),
                            tag='fit')
        formula <- y ~ 1 + V1 + V2 + V3 + V4 + f(VC, model = "iid") + f(spatial.field, model = spde)

    }

    if (n_vars == 5){
        stk <- inla.stack(data=list(y = as.integer(unlist(df_PA[,1]))),
                            effects=list(c(list(Intercept=1), iset),
                                           list(VC = unlist(df_PA[,2]), V1 = unlist(df_PA[,vars[[1]]]), V2 = unlist(df_PA[,vars[[2]]]), V3 = unlist(df_PA[,vars[[3]]]), V4 = unlist(df_PA[,vars[[4]]]), V5 = unlist(df_PA[,vars[[5]]])
                                        )
                                    ), 
                            A=list(A,1),
                            tag='fit')
        formula <- y ~ 1 + V1 + V2 + V3 + V4 + V5 + f(VC, model = "iid") + f(spatial.field, model = spde)

    }

    if (n_vars == 6){
        stk <- inla.stack(data=list(y = as.integer(unlist(df_PA[,1]))),
                            effects=list(c(list(Intercept=1), iset),
                                           list(VC = unlist(df_PA[,2]), V1 = unlist(df_PA[,vars[[1]]]), V2 = unlist(df_PA[,vars[[2]]]), V3 = unlist(df_PA[,vars[[3]]]), V4 = unlist(df_PA[,vars[[4]]]), V5 = unlist(df_PA[,vars[[5]]]), V6 = unlist(df_PA[,vars[[6]]])
                                        )
                                    ), 
                            A=list(A,1),
                            tag='fit')
        formula <- y ~ 1 + V1 + V2 + V3 + V4 + V5 + V6 + f(VC, model = "iid") + f(spatial.field, model = spde)

    }

    if (n_vars == 7){
        stk <- inla.stack(data=list(y = as.integer(unlist(df_PA[,1]))),
                            effects=list(c(list(Intercept=1), iset),
                                           list(VC = unlist(df_PA[,2]), V1 = unlist(df_PA[,vars[[1]]]), V2 = unlist(df_PA[,vars[[2]]]), V3 = unlist(df_PA[,vars[[3]]]), V4 = unlist(df_PA[,vars[[4]]]), V5 = unlist(df_PA[,vars[[5]]]), V6 = unlist(df_PA[,vars[[6]]]), V7 = unlist(df_PA[,vars[[7]]])
                                        )
                                    ), 
                            A=list(A,1),
                            tag='fit')
        formula <- y ~ 1 + V1 + V2 + V3 + V4 + V5 + V6 + V7 + f(VC, model = "iid") + f(spatial.field, model = spde)
    }

    if (n_vars == 8){
        stk <- inla.stack(data=list(y = as.integer(unlist(df_PA[,1]))),
                            effects=list(c(list(Intercept=1), iset),
                                           list(VC = unlist(df_PA[,2]), V1 = unlist(df_PA[,vars[[1]]]), V2 = unlist(df_PA[,vars[[2]]]), V3 = unlist(df_PA[,vars[[3]]]), V4 = unlist(df_PA[,vars[[4]]]), V5 = unlist(df_PA[,vars[[5]]]), V6 = unlist(df_PA[,vars[[6]]]), V7 = unlist(df_PA[,vars[[7]]]), V8 = unlist(df_PA[,vars[[8]]])
                                        )
                                    ), 
                            A=list(A,1),
                            tag='fit')
        formula <- y ~ 1 + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + f(VC, model = "iid") + f(spatial.field, model = spde)
    }

    if (n_vars == 9){
        stk <- inla.stack(data=list(y = as.integer(unlist(df_PA[,1]))),
                            effects=list(c(list(Intercept=1), iset),
                                           list(VC = unlist(df_PA[,2]), V1 = unlist(df_PA[,vars[[1]]]), V2 = unlist(df_PA[,vars[[2]]]), V3 = unlist(df_PA[,vars[[3]]]), V4 = unlist(df_PA[,vars[[4]]]), V5 = unlist(df_PA[,vars[[5]]]), V6 = unlist(df_PA[,vars[[6]]]), V7 = unlist(df_PA[,vars[[7]]]), V8 = unlist(df_PA[,vars[[8]]]), V9 = unlist(df_PA[,vars[[9]]])
                                        )
                                    ), 
                            A=list(A,1),
                            tag='fit')
        formula <- y ~ 1 + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + f(VC, model = "iid") + f(spatial.field, model = spde)

    }
    
    if (n_vars == 10){
        stk <- inla.stack(data=list(y = as.integer(unlist(df_PA[,1]))),
                            effects=list(c(list(Intercept=1), iset),
                                           list(VC = unlist(df_PA[,2]), V1 = unlist(df_PA[,vars[[1]]]), V2 = unlist(df_PA[,vars[[2]]]), V3 = unlist(df_PA[,vars[[3]]]), V4 = unlist(df_PA[,vars[[4]]]), V5 = unlist(df_PA[,vars[[5]]]), V6 = unlist(df_PA[,vars[[6]]]), V7 = unlist(df_PA[,vars[[7]]]), V8 = unlist(df_PA[,vars[[8]]]), V9 = unlist(df_PA[,vars[[9]]]), V10 = unlist(df_PA[,vars[[10]]])
                                        )
                                    ), 
                            A=list(A,1),
                            tag='fit')
        formula <- y ~ 1 + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + f(VC, model = "iid") + f(spatial.field, model = spde)
    }

    if (n_vars == 11){
        stk <- inla.stack(data=list(y = as.integer(unlist(df_PA[,1]))),
                            effects=list(c(list(Intercept=1), iset),
                                           list(VC = unlist(df_PA[,2]), V1 = unlist(df_PA[,vars[[1]]]), V2 = unlist(df_PA[,vars[[2]]]), V3 = unlist(df_PA[,vars[[3]]]), V4 = unlist(df_PA[,vars[[4]]]), V5 = unlist(df_PA[,vars[[5]]]), V6 = unlist(df_PA[,vars[[6]]]), V7 = unlist(df_PA[,vars[[7]]]), V8 = unlist(df_PA[,vars[[8]]]), V9 = unlist(df_PA[,vars[[9]]]), V10 = unlist(df_PA[,vars[[10]]]), V11 = unlist(df_PA[,vars[[11]]])
                                        )
                                    ), 
                            A=list(A,1),
                            tag='fit')
        formula <- y ~ 1 + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + f(VC, model = "iid") + f(spatial.field, model = spde)
    }

    if (n_vars == 12){
        stk <- inla.stack(data=list(y = as.integer(unlist(df_PA[,1]))),
                            effects=list(c(list(Intercept=1), iset),
                                           list(VC = unlist(df_PA[,2]), V1 = unlist(df_PA[,vars[[1]]]), V2 = unlist(df_PA[,vars[[2]]]), V3 = unlist(df_PA[,vars[[3]]]), V4 = unlist(df_PA[,vars[[4]]]), V5 = unlist(df_PA[,vars[[5]]]), V6 = unlist(df_PA[,vars[[6]]]), V7 = unlist(df_PA[,vars[[7]]]), V8 = unlist(df_PA[,vars[[8]]]), V9 = unlist(df_PA[,vars[[9]]]), V10 = unlist(df_PA[,vars[[10]]]), V11 = unlist(df_PA[,vars[[11]]]), V12 = unlist(df_PA[,vars[[12]]])
                                        )
                                    ), 
                            A=list(A,1),
                            tag='fit')
        formula <- y ~ 1 + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + f(VC, model = "iid") + f(spatial.field, model = spde)
    }

    if (n_vars == 13){
        stk <- inla.stack(data=list(y = as.integer(unlist(df_PA[,1]))),
                            effects=list(c(list(Intercept=1), iset),
                                           list(VC = unlist(df_PA[,2]), V1 = unlist(df_PA[,vars[[1]]]), V2 = unlist(df_PA[,vars[[2]]]), V3 = unlist(df_PA[,vars[[3]]]), V4 = unlist(df_PA[,vars[[4]]]), V5 = unlist(df_PA[,vars[[5]]]), V6 = unlist(df_PA[,vars[[6]]]), V7 = unlist(df_PA[,vars[[7]]]), V8 = unlist(df_PA[,vars[[8]]]), V9 = unlist(df_PA[,vars[[9]]]), V10 = unlist(df_PA[,vars[[10]]]), V11 = unlist(df_PA[,vars[[11]]]), V12 = unlist(df_PA[,vars[[12]]]), V13 = unlist(df_PA[,vars[[13]]])
                                        )
                                    ), 
                            A=list(A,1),
                            tag='fit')
        formula <- y ~ 1 + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + f(VC, model = "iid") + f(spatial.field, model = spde)
    }
    
    if (n_vars == 14){
        stk <- inla.stack(data=list(y = as.integer(unlist(df_PA[,1]))),
                            effects=list(c(list(Intercept=1), iset),
                                           list(VC = unlist(df_PA[,2]), V1 = unlist(df_PA[,vars[[1]]]), V2 = unlist(df_PA[,vars[[2]]]), V3 = unlist(df_PA[,vars[[3]]]), V4 = unlist(df_PA[,vars[[4]]]), V5 = unlist(df_PA[,vars[[5]]]), V6 = unlist(df_PA[,vars[[6]]]), V7 = unlist(df_PA[,vars[[7]]]), V8 = unlist(df_PA[,vars[[8]]]), V9 = unlist(df_PA[,vars[[9]]]), V10 = unlist(df_PA[,vars[[10]]]), V11 = unlist(df_PA[,vars[[11]]]), V12 = unlist(df_PA[,vars[[12]]]), V13 = unlist(df_PA[,vars[[13]]]), V14 = unlist(df_PA[,vars[[14]]])
                                        )
                                    ), 
                            A=list(A,1),
                            tag='fit')
        formula <- y ~ 1 + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + f(VC, model = "iid") + f(spatial.field, model = spde)
    }
        
    # build the model
    model<-inla(formula,
                data=inla.stack.data(stk,spde=spde), # data plus spde
                family= 'binomial', # data family
                control.family = list(link = "logit"), # logit for logistic regression
                control.predictor=list(link = 1, A=inla.stack.A(stk)), # calculate the covariate weights
                control.compute = list(waic = TRUE) # calculate Watanabe-Akaike information criterion 
                )
    
    # get waic
    waic <- model$waic$waic
        
    # write waic to file
    cat(c(i, waic, seeds[i], unlist(combs[i,]), '\n'), file= "../results/waic_AfrRev.txt",append=TRUE)
    
    # save the model
    save(model, file = paste0("../models/AfrRev/", toString(i), ".Rda"))

}
