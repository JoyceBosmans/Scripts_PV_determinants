# see email Konrad 24 june with file_joyce.R
pacman::p_load(readxl,INLA)
#inla.setOption(inla.mode="experimental") 

dir_Konrad <- '/vol/milkunarc2/kmielke/pv' 

# load df_PA (data frame)
load(file = file.path(dir_Konrad,'/data/rda/df_PA_Afr_country_proprocessed.Rda'))

# load model performance file - notice to get the *Rev.txt version
model_performance <- read.csv(file.path(dir_Konrad,'/waic_AfrRev.txt'), sep = ' ', header = FALSE)

# select best model
best_model <- model_performance[which.min(model_performance$V2),]
print(best_model)
print('check column numbers against row 40 in script preprocessing_Afr.R')
#df_PA <- df_PA[,c('PA_bool', 'SOVEREIGN1', 'travel_log', 'slope_log', 'elev_log', 'road_log', 'grid_log', 'rsds', 'protect', 'Agriculture', 'Forest', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water', 'x_coord', 'y_coord')]

# order all models by WAIC ($V2) and find how many are within 2 WAIC:
models_within_2WAIC <- model_performance[order(model_performance$V2),]
models_within_2WAIC <- models_within_2WAIC[models_within_2WAIC$V2 < best_model[,2] + 2,]
print(paste('Number of models within 2 waic of the best model:',nrow(models_within_2WAIC)))

# load best model and copy to this dir - notice to use *Rev dir
print(paste('best model is model #',toString(best_model[,1]),', copy to local dir'))
rda_file <- file.path(dir_Konrad,paste0('/models/AfrRev/', toString(best_model[,1]), '.Rda'))
load(rda_file)
file.copy(rda_file,'.')

# ===> find the best model without Forest ('11' in model-performance)! was included but shouldn't have been due to multicollinearity
model_performance_no_forest <- subset(model_performance,V4 != 11 & V5 != 11 & V6 != 11 & V7 != 11 & V8 != 11 & V9 != 11 & V10 != 11 & V11 != 11)
best_model_no_forest        <- model_performance_no_forest[which.min(model_performance_no_forest$V2),]

models_within_2WAIC <- model_performance_no_forest[order(model_performance_no_forest$V2),]
models_within_2WAIC <- models_within_2WAIC[models_within_2WAIC$V2 < best_model[,2] + 2,]
print(paste('Number of models within 2 waic of the best model:',nrow(models_within_2WAIC)))

rda_file_no_forest <- file.path(dir_Konrad,paste0('/models/AfrRev/', toString(best_model_no_forest[,1]), '.Rda'))
load(rda_file_no_forest)
file.copy(rda_file_no_forest,'.')


# get model summary
summary(model)

# store table with intercept and coefficients - consistent for all continents so include all predictors
rows <- c('intercept','travel_log', 'slope_log', 'elev_log', 'road_log', 'grid_log', 'rsds', 'protect', 'Agriculture', 'Forest', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water')
data <- c( -2.756,-1.936,-0.204, 0,-0.674,-0.550,0.810, 0,0.521, 0,0.797, 0, 0,1.457, 0)

tab_coef_Afr <- matrix(data, ncol=1, byrow=TRUE)
rownames(tab_coef_Afr) <- rows
colnames(tab_coef_Afr) <- c('coefficient')
tab_coef_Afr <- as.table(tab_coef_Afr)		# get a coefficient: tab['rsds',]

save(tab_coef_Afr,file='tab_coef_Afr.Rda')

#~ V1 <- 'PA_bool'			coefficients of best model: 
#~ V2 <- 'SOVEREIGN1'		XXX   -2.756 INTERCEPT
#~ V3 <- 'travel_log' 		XXX   -1.936
#~ V4 <- 'slope_log'  		XXX   -0.204
#~ V5 <- 'elev_log'   		
#~ V6 <- 'road_log'         XXX   -0.674 
#~ V7 <- 'grid_log'         XXX   -0.550 
#~ V8 <- 'rsds'       		XXX    0.810
#~ V9 <- 'protect'    		
#~ V10 <- 'Agriculture' 	XXX    0.521
#~ V11 <- 'Forest'
#~ V12 <- 'Short_natural' 	XXX    0.797
#~ V13 <- 'Wetland'         
#~ V14 <- 'Urban'			
#~ V15 <- 'Bare'            XXX    1.457
#~ V16 <- 'Water'           
#~ V17 <- 'x_coord'
#~ V18 <- 'y_coord'

