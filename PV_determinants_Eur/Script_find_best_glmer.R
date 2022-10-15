pacman::p_load(lme4,MuMIn,readxl,dplyr)
source('/vol/milkunB/jbosmans/SolarWindPotentials/Function_evaluate_model.R')

df_PA <- read_xlsx('df_PA_Eur_country_261121.xlsx')

# based on distributions (see Script_predictor_stats.R), log-transform road-dist and travel-time
print('NOTE: grid distance now in KM!')
df_PA$road_log   <- log10(df_PA$road_dist + 1)	# shift away from 0 to avoid -inf in log-transformed values
df_PA$travel_log <- log10(df_PA$access + 1)
df_PA$grid_log   <- log10(df_PA$grid_km + 1)
df_PA$slope_log  <- log10(df_PA$slope)
df_PA$elev_log   <- log10(df_PA$elev + abs(min(df_PA$elev))+1)

df_scaled <- df_PA
df_scaled[,c('road_log', 'grid_log','travel_log', 'slope_log', 'elev_log', 'rsds', 'Forest', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water', 'available_area')] <- scale(df_PA[,c('road_log','grid_log','travel_log', 'slope_log', 'elev_log', 'rsds', 'Forest', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water', 'available_area')],center=TRUE,scale=TRUE)

# model with all predictors, all scaled, created in Script_glmer.R: 
print('NOTE: using model based on scaled predictors!')
load('glmer_model_scaled.rda')
summary(model_scaled)

# scaled model selection / dredging -----------------------------------------------------------
print('model dredging')
options(na.action = "na.fail")
bestmodels <- dredge(model_scaled,rank=AIC)
subset(bestmodels, delta < 10)	# show models within 10 AIC of best model
importance(bestmodels)			# variable importance weights based on all models and their weight
options(na.action = "na.omit")

# find the best model and print its summary and test statistics ------------------------------- 
topmodel  <- get.models(bestmodels, subset=1)[[1]]

summary(topmodel)
save(topmodel, file='topmodel.rda')
save(bestmodels, file='bestmodels.rda')

print('   test statistics for best scaled model')
eval <- evaluate_model(topmodel,df_PA)
print(eval)
r.squaredGLMM(topmodel)

res_all_scaled <- residuals(topmodel) 

# try model averaging
avg_model <- model.avg(bestmodels, subset = delta < 2,fit=TRUE)
summary(avg_model)
save(avg_model,file='avg_model.rda')
