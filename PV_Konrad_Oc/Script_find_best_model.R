# see email Konrad 24 june with file_joyce.R
pacman::p_load(readxl,INLA)
#inla.setOption(inla.mode="experimental") 

dir_Konrad <- '/vol/milkunarc2/kmielke/pv' 

# load df_PA (data frame)
load(file = file.path(dir_Konrad,'/data/rda/df_PA_Oc_country_proprocessed.Rda'))

# load model performance file - notice to get the *Rev.txt version
#model_performance <- read.csv(file.path(dir_Konrad,'/waic_OcRev.txt'), sep = ' ', header = FALSE)
model_performance <- read.csv('waic_OcRev.txt', sep = ' ', header = FALSE)

# select best model
best_model <- model_performance[which.min(model_performance$V2),]
print(best_model)
print('check column numbers against row 40 in script preprocessing_Oc.R')
#df_PA <- df_PA[,c('PA_bool', 'SOVEREIGN1', 'travel_log', 'slope_log', 'elev_log', 'road_log', 'grid_log', 'rsds', 'protect', 'Agriculture', 'Forest', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water', 'x_coord', 'y_coord')]

# order all models by WAIC ($V2) and find how many are within 2 WAIC:
models_within_2WAIC <- model_performance[order(model_performance$V2),]
models_within_2WAIC <- models_within_2WAIC[models_within_2WAIC$V2 < best_model[,2] + 2,]
print(paste('Number of models within 2 waic of the best model:',nrow(models_within_2WAIC)))

# load best model and copy to this dir - notice to use *Rev dir
print(paste('best model is model #',toString(best_model[,1]),', copy to local dir'))
rda_file <- file.path(dir_Konrad,paste0('/models/OcRev/', toString(best_model[,1]), '.Rda'))
load(rda_file)
file.copy(rda_file,'.')

# ===> none of these have forest in them

# get model summary
summary(model)

# store table with intercept and coefficients - consistent for all continents so include all predictors
rows <- c('intercept','travel_log', 'slope_log', 'elev_log', 'road_log', 'grid_log', 'rsds', 'protect', 'Agriculture', 'Forest', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water')
data <- c( -3.230,-1.728,0, -0.415,-1.043,-1.009,0, 0,0, 0,0, 0.205, -0.500,0, 0)

tab_coef_Oc <- matrix(data, ncol=1, byrow=TRUE)
rownames(tab_coef_Oc) <- rows
colnames(tab_coef_Oc) <- c('coefficient')
tab_coef_Oc <- as.table(tab_coef_Oc)		# get a coefficient: tab['rsds',]

save(tab_coef_Oc,file='tab_coef_Oc.Rda')

#~ V1 <- 'PA_bool'			coefficients of best model: 
#~ V2 <- 'SOVEREIGN1'		XXX   -3.230 INTERCEPT
#~ V3 <- 'travel_log' 		XXX   -1.728 
#~ V4 <- 'slope_log'  		
#~ V5 <- 'elev_log'   		XXX   -0.415
#~ V6 <- 'road_log'         XXX   -1.043 
#~ V7 <- 'grid_log'         XXX   -1.009
#~ V8 <- 'rsds'       		 
#~ V9 <- 'protect'    		
#~ V10 <- 'Agriculture' 	
#~ V11 <- 'Forest'
#~ V12 <- 'Short_natural' 	
#~ V13 <- 'Wetland'         XXX    0.205
#~ V14 <- 'Urban'			XXX   -0.500
#~ V15 <- 'Bare'           
#~ V16 <- 'Water'           
#~ V17 <- 'x_coord'
#~ V18 <- 'y_coord'


