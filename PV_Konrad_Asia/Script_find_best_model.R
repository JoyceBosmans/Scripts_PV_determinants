# see email Konrad 24 june with file_joyce.R
pacman::p_load(readxl,INLA)
#inla.setOption(inla.mode="experimental") 

dir_Konrad <- '/vol/milkunarc2/kmielke/pv' 

# load df_PA (data frame)
load(file = file.path(dir_Konrad,'/data/rda/df_PA_Asia_country_060622_proprocessed_updated.Rda'))

# load model performance file - notice to get the *Rev.txt version
model_performance <- read.csv('waic_AsiaRev_updated.txt', sep = ' ', header = FALSE)

# select best model
best_model <- model_performance[which.min(model_performance$V2),]
print(best_model)
print('check column numbers against row 40 in script preprocessing_Asia.R')
#df_PA <- df_PA[,c('PA_bool', 'SOVEREIGN1', 'travel_log', 'slope_log', 'elev_log', 'road_log', 'grid_log', 'rsds', 'protect', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water', 'x_coord', 'y_coord')]

# order all models by WAIC ($V2) and find how many are within 2 WAIC:
models_within_2WAIC <- model_performance[order(model_performance$V2),]
models_within_2WAIC <- models_within_2WAIC[models_within_2WAIC$V2 < best_model[,2] + 2,]
print(paste('Number of models within 2 waic of the best model:',nrow(models_within_2WAIC)))

# load best model and copy to this dir - notice to use *Rev dir
print(paste('best model is model #',toString(best_model[,1]),', copy to local dir'))
rda_file <- file.path(dir_Konrad,paste0('/models/AsiaRev_updated/', toString(best_model[,1]), '.Rda'))
load(rda_file)
file.copy(rda_file,'.')

# get model summary
summary(model)

# store table with intercept and coefficients - consistent for all continents so include all predictors
rows <- c('intercept','travel_log', 'slope_log', 'elev_log', 'road_log', 'grid_log', 'rsds', 'protect', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water')
data <- c(-4.755,-0.705, 0.106,-0.256, -0.487,-0.363 ,1.869,-0.122, 0.937,1.376,0.412,-0.084,1.667,0.262)

tab_coef_Asia <- matrix(data, ncol=1, byrow=TRUE)
rownames(tab_coef_Asia) <- rows
colnames(tab_coef_Asia) <- c('coefficient')
tab_coef_Asia <- as.table(tab_coef_Asia)		# get a coefficient: tab['rsds',]

save(tab_coef_Asia,file='tab_coef_Asia.Rda')

#~ V1 <- 'PA_bool'			coefficients of best model: 
#~ V2 <- 'SOVEREIGN1'		XXX -4.755 INTERCEPT
#~ V3 <- 'travel_log' 		XXX -0.705
#~ V4 <- 'slope_log'  		XXX  0.106
#~ V5 <- 'elev_log'   		XXX -0.256
#~ V6 <- 'road_log' 		XXX -0.487
#~ V7 <- 'grid_log'         XXX -0.363 
#~ V8 <- 'rsds'       		XXX  1.869
#~ V9 <- 'protect'    		XXX -0.122
#~ V10 <- 'Agriculture' 	XXX  0.937	(relative to forest!)
#~ V11 <- 'Short_natural' 	XXX  1.376
#~ V12 <- 'Wetland'         XXX  0.412
#~ V13 <- 'Urban'			XXX -0.084
#~ V14 <- 'Bare'            XXX  1.667
#~ V15 <- 'Water'           XXX  0.262
#~ V15 <- 'x_coord'
#~ V16 <- 'y_coord'
