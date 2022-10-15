# see email Konrad 24 june with file_joyce.R
pacman::p_load(readxl,INLA)

dir_Konrad <- '/vol/milkunarc2/kmielke/pv' 

# load df_PA (data frame)
load(file = file.path(dir_Konrad,'/data/rda/df_PA_SAm_country_060622_proprocessed.Rda'))

# load model performance file - notice to get the *Rev.txt version
model_performance <- read.csv(file.path(dir_Konrad,'/results/waic_SAmRev.txt'), sep = ' ', header = FALSE)

# select best model
best_model <- model_performance[which.min(model_performance$V2),]
print(best_model)
print('check column numbers against row 38 in script preprocessing_SAm.R')
#df_PA <- df_PA[,c('PA_bool', 'SOVEREIGN1', 'travel_log', 'slope_log', 'elev_log', 'road_log', 'grid_log', 'rsds', 'protect', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water', 'x_coord', 'y_coord')]
#df_PA <- df_PA[,c('PA_bool', 'SOVEREIGN1', 'travel_log', 'slope_log', 'elev_log', 'road_log', 'grid_log', 'rsds', 'protect', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water', 'x_coord', 'y_coord')]

# order all models by WAIC ($V2) and find how many are within 2 WAIC:
models_within_2WAIC <- model_performance[order(model_performance$V2),]
models_within_2WAIC <- models_within_2WAIC[models_within_2WAIC$V2 < best_model[,2] + 2,]
print(paste('Number of models within 2 waic of the best model:',nrow(models_within_2WAIC)))

# load best model and copy to this dir
print(paste('best model is model #',toString(best_model[,1]),', copy to local dir'))
rda_file <- file.path(dir_Konrad,paste0('/models/SAmRev/', toString(best_model[,1]), '.Rda'))
load(rda_file)
file.copy(rda_file,'.')

# get model summary
summary(model)

rows <- c('intercept','travel_log', 'slope_log', 'elev_log', 'road_log', 'grid_log', 'rsds', 'protect', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water')
data <- c(-3.032, -1.786, -0.541, 0, -0.479, -0.450, 1.147, 0, 0, 0, 0, 0, 0, 0)

tab_coef_SAm <- matrix(data, ncol=1, byrow=TRUE)
rownames(tab_coef_SAm) <- rows
colnames(tab_coef_SAm) <- c('coefficient')
tab_coef_SAm <- as.table(tab_coef_SAm)		# get a coefficient: tab['rsds',]

save(tab_coef_SAm,file='tab_coef_SAm.Rda')


#~ V1 <- 'PA_bool'			coefficients of best model: 
#~ V2 <- 'SOVEREIGN1'		XXX -3.032 intercept
#~ V3 <- 'travel_log' 		XXX -1.786
#~ V4 <- 'slope_log'  		XXX -0.541
#~ V5 <- 'elev_log'   		
#~ V6 <- 'road_log'			XXX -0.479
#~ V7 <- 'grid_log'			XXX -0.450
#~ V8 <- 'rsds'       		XXX  1.147
#~ V9 <- 'protect'    		
#~ V10 <- 'Agriculture' 	
#~ V11 <- 'Short_natural' 	
#~ V12 <- 'Wetland'     
#~ V13 <- 'Urban'			
#~ V14 <- 'Bare'       
#~ V15 <- 'Water'
#~ V15 <- 'x_coord'
#~ V16 <- 'y_coord'
