# see email Konrad 24 june with file_joyce.R
pacman::p_load(readxl,INLA)
#inla.setOption(inla.mode="experimental") 

dir_Konrad <- '/vol/milkunarc2/kmielke/pv' 

# load df_PA (data frame)
load(file = file.path(dir_Konrad,'/data/rda/df_PA_NAm_country_060622_proprocessed.Rda'))

# load model performance file - notice to get the *Rev.txt version
model_performance <- read.csv(file.path(dir_Konrad,'/results/waic_NAmRev.txt'), sep = ' ', header = FALSE)

# select best model
best_model <- model_performance[which.min(model_performance$V2),]
print(best_model)
print('check column numbers against row 40 in script preprocessing_NAm.R')
#df_PA <- df_PA[,c('PA_bool', 'SOVEREIGN1', 'travel_log', 'slope_log', 'elev_log', 'road_log', 'grid_log', 'rsds', 'protect', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water', 'x_coord', 'y_coord')]

# order all models by WAIC ($V2) and find how many are within 2 WAIC:
models_within_2WAIC <- model_performance[order(model_performance$V2),]
models_within_2WAIC <- models_within_2WAIC[models_within_2WAIC$V2 < best_model[,2] + 2,]
print(paste('Number of models within 2 waic of the best model:',nrow(models_within_2WAIC)))

# load best model and copy to this dir - notice to use *Rev dir
print(paste('best model is model #',toString(best_model[,1]),', copy to local dir'))
rda_file <- file.path(dir_Konrad,paste0('/models/NAmRev/', toString(best_model[,1]), '.Rda'))
load(rda_file)
file.copy(rda_file,'.')

# ===> find the best model without road_dist ('6' in model-performance)! was included but shouldn't have been due to multicollinearity
model_performance_no_road <- subset(model_performance,V4 != 6 & V5 != 6 & V6 != 6 & V7 != 6)
best_model_no_road        <- model_performance_no_road[which.min(model_performance_no_road$V2),]

rda_file_no_road <- file.path(dir_Konrad,paste0('/models/NAmRev/', toString(best_model_no_road[,1]), '.Rda'))
load(rda_file_no_road)
file.copy(rda_file_no_road,'.')
# next best model without road-dist is 8187 (also follows from ordering model_performance by WAIC)

# get model summary
summary(model)

# store table with intercept and coefficients - consistent for all continents so include all predictors
rows <- c('intercept','travel_log', 'slope_log', 'elev_log', 'road_log', 'grid_log', 'rsds', 'protect', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water')
data <- c( -4.125,-1.467,-0.354,-0.321, 0,-0.357 ,2.184, -0.595,0.731,0.906,0.082,-0.099,0.193,-0.450)

tab_coef_NAm <- matrix(data, ncol=1, byrow=TRUE)
rownames(tab_coef_NAm) <- rows
colnames(tab_coef_NAm) <- c('coefficient')
tab_coef_NAm <- as.table(tab_coef_NAm)		# get a coefficient: tab['rsds',]

save(tab_coef_NAm,file='tab_coef_NAm.Rda')

#~ V1 <- 'PA_bool'			coefficients of best model: 
#~ V2 <- 'SOVEREIGN1'		XXX   -4.125 INTERCEPT
#~ V3 <- 'travel_log' 		XXX   -1.467
#~ V4 <- 'slope_log'  		XXX   -0.354
#~ V5 <- 'elev_log'   		XXX   -0.321
#~ V6 <- 'road_log'         
#~ V7 <- 'grid_log'         XXX   -0.357 
#~ V8 <- 'rsds'       		XXX    2.184
#~ V9 <- 'protect'    		XXX   -0.595
#~ V10 <- 'Agriculture' 	XXX    0.731 (relative to forest!)
#~ V11 <- 'Short_natural' 	XXX    0.906
#~ V12 <- 'Wetland'         XXX    0.082
#~ V13 <- 'Urban'			XXX   -0.099
#~ V14 <- 'Bare'            XXX    0.193
#~ V15 <- 'Water'           XXX   -0.450
#~ V15 <- 'x_coord'
#~ V16 <- 'y_coord'
