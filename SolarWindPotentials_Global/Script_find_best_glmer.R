pacman::p_load(rgdal, readxl, writexl, ggplot2, spData, sp, sf, raster,dplyr,mapview,caret,PresenceAbsence,maps,lme4,MuMIn)
source('/vol/milkunB/jbosmans/SolarWindPotentials/Function_evaluate_model.R')

# find best glmer (no weights, with mumin dredge, country as mixed effect) 

# dataframe is created in Script_PA_predictors.R, and statistics are checked in Script_predictor_stats.R
df_PA <- read_xlsx('df_PA_global_country.xlsx')

# based on distributions (see Script_predictor_stats.R), log-transform road-dist and travel-time
df_PA$road_log   <- log10(df_PA$road_dist + 1)	# shift away from 0 to avoid -inf in log-transformed values
df_PA$travel_log <- log10(df_PA$access + 1)
print(paste('length of input df:',nrow(df_PA)))

colnames(df_PA)[colnames(df_PA) == 'grid'] <- 'grid_proximity'
colnames(df_PA)[colnames(df_PA) == 'access'] <- 'travel_time'
colnames(df_PA)[colnames(df_PA) == 'rsds'] <- 'irradiance'

df_scaled <- df_PA
df_scaled[,c('grid_proximity', 'travel_time', 'slope', 'elev', 'irradiance', 'Forest', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'available_area','road_dist','urban_dist')] <- scale(df_PA[,c('grid_proximity', 'travel_time', 'slope', 'elev', 'irradiance', 'Forest', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'available_area','road_dist','urban_dist')],center=TRUE,scale=TRUE)

print('build a glmer on all input, log-transformed travel time and road distance')
print('based on vifs, forest is removed from the predictors')

#glmer_model <- glmer(PA_bool ~ road_log + urban_dist + grid_proximity + travel_log + slope + elev + protect + irradiance + Agriculture + Short_natural + Wetland + Urban + Bare + Water + (1|SOVEREIGN1), data = df_PA, family = 'binomial'(link = 'logit'))

#~ glmer_model_scaled <- glmer(PA_bool ~ road_log + urban_dist + grid_proximity + travel_log + slope + elev + protect + irradiance + Agriculture + Short_natural + Wetland + Urban + Bare + Water + (1|SOVEREIGN1), data = df_scaled, family = 'binomial'(link = 'logit'),control=glmerControl(optCtrl=list(maxfun=5e4)))

glmer_model_scaled <- glmer(PA_bool ~ urban_dist + grid_proximity + travel_log + slope + elev + protect + irradiance + Agriculture + Short_natural +  Urban + Bare + (1|SOVEREIGN1), data = df_scaled, family = 'binomial'(link = 'logit'),control=glmerControl(optCtrl=list(maxfun=5e4)))

print('March 2021: rerun with subset of predictors. road_log, Wetland, Water removed based on coefficient values in glmer_all.txt')

#~ # model selection / dredging -----------------------------------------------------------
#~ print('model dredging for original model')
#~ options(na.action = "na.fail")
#~ bestmodel <- dredge(glmer_model,rank=AIC)
#~ subset(bestmodel, delta < 10)	# show models within 10 AIC of best model
#~ importance(bestmodel)			# variable importance weights based on all models and their weight
#~ options(na.action = "na.omit")	

#~ # SCALED model selection / dredging -----------------------------------------------------------
print('model dredging for scaled model')
options(na.action = "na.fail")
bestmodel <- dredge(glmer_model_scaled,rank=AIC)
subset(bestmodel, delta < 10)	# show models within 10 AIC of best model
importance(bestmodel)			# variable importance weights based on all models and their weight
options(na.action = "na.omit")
