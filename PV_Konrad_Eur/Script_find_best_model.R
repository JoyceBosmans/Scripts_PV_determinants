# see email Konrad 24 june with file_joyce.R
pacman::p_load(readxl,INLA)
#inla.setOption(inla.mode="experimental") 

dir_Konrad <- '/vol/milkunarc2/kmielke/pv' 

# load df_PA (data frame)
load(file = file.path(dir_Konrad,'/data/rda/df_PA_Eur_country_060622_proprocessed.Rda'))

# load model performance file - notice to get the *Rev.txt version
model_performance <- read.csv(file.path(dir_Konrad,'/results/waic_EurRev.txt'), sep = ' ', header = FALSE)

# select best model
best_model <- model_performance[which.min(model_performance$V2),]
print(best_model)
print('check column numbers against row 38 in script preprocessing_Eur.R')
#df_PA <- df_PA[,c('PA_bool', 'SOVEREIGN1', 'travel_log', 'slope_log', 'elev_log', 'road_log', 'rsds', 'protect', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water', 'x_coord', 'y_coord')]

# order all models by WAIC ($V2) and find how many are within 2 WAIC:
models_within_2WAIC <- model_performance[order(model_performance$V2),]
models_within_2WAIC <- models_within_2WAIC[models_within_2WAIC$V2 < best_model[,2] + 2,]
print(paste('Number of models within 2 waic of the best model:',nrow(models_within_2WAIC)))

# load best model and copy to this dir - notice to use *Rev dir
print(paste('best model is model #',toString(best_model[,1]),', copy to local dir'))
rda_file <- file.path(dir_Konrad,paste0('/models/EurRev/', toString(best_model[,1]), '.Rda'))
load(rda_file)
file.copy(rda_file,'.')

# get model summary
summary(model)

# store table with intercept and coefficients - consistent for all continents so include all predictors
rows <- c('intercept','travel_log', 'slope_log', 'elev_log', 'road_log', 'grid_log', 'rsds', 'protect', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water')
data <- c(-3.452, -0.743, -0.396, -0.122, 0, 0, 1.618, -0.411, 0.447, 0.201, 0, -0.045, 0, 0)

tab_coef_Eur <- matrix(data, ncol=1, byrow=TRUE)
rownames(tab_coef_Eur) <- rows
colnames(tab_coef_Eur) <- c('coefficient')
tab_coef_Eur <- as.table(tab_coef_Eur)		# get a coefficient: tab['rsds',]

save(tab_coef_Eur,file='tab_coef_Eur.Rda')

#~ V1 <- 'PA_bool'			coefficients of best model: 
#~ V2 <- 'SOVEREIGN1'		XXX -3.452 INTERCEPT
#~ V3 <- 'travel_log' 		XXX -0.743
#~ V4 <- 'slope_log'  		XXX -0.396
#~ V5 <- 'elev_log'   		XXX -0.122
#~ V6 <- 'road_log'
#~ V7 <- 'rsds'       		XXX  1.618
#~ V8 <- 'protect'    		XXX -0.411
#~ V9 <- 'Agriculture' 		XXX  0.447	(relative to forest!)
#~ V10 <- 'Short_natural' 	XXX  0.201
#~ V11 <- 'Wetland'     
#~ V12 <- 'Urban'			XXX -0.045
#~ V13 <- 'Bare'       
#~ V14 <- 'Water'
#~ V15 <- 'x_coord'
#~ V16 <- 'y_coord'
