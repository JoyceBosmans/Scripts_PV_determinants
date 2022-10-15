pacman::p_load(ggplot2, writexl,data.table,readxl)

dir_Konrad <- '/vol/milkunarc2/kmielke/pv' 

# load df_PA (data frame), in which predictors are already log-transformed and standardized
load(file = file.path(dir_Konrad,'/data/rda/df_PA_Eur_country_060622_proprocessed.Rda'))
df_PA_orig <- read_xlsx('../PV_determinants_Eur/df_PA_Eur_country_060622.xlsx')

# predictor importance as computed in Script_predictor_importance.R
df_PI <- read_xlsx('df_PI_Eur.xlsx')

# since we worked with normalized data, all mean values are 0. So applying the model for partial dependency plots is simly only including the relevant predictor

# get intercept and coefficients from Script_find_best_model.R
predict_rsds     <- -3.452 + 1.618*df_PA$rsds
probability_rsds <- 1/(1 + exp(-predict_rsds))
quantile(probability_rsds)
plot(df_PA_orig$rsds,probability_rsds)

predict_travel     <- -3.451 - 0.743*df_PA$travel_log
probability_travel <- 1/(1 + exp(-predict_travel))
quantile(probability_travel)
plot(df_PA_orig$access,probability_travel)

predict_Agriculture     <- -3.451 + 0.447*df_PA$Agriculture
probability_Agriculture <- 1/(1 + exp(-predict_Agriculture))
quantile(probability_Agriculture)
plot(df_PA_orig$Agriculture,probability_Agriculture)

#~ V1 <- 'PA_bool'			coefficients of best model: 
#~ V2 <- 'SOVEREIGN1'		XXX -3.452 intercept
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
