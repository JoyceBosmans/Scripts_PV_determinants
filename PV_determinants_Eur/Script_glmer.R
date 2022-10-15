pacman::p_load(rgdal, readxl, writexl, ggplot2, spData, sp, sf, lme4,MuMIn,dplyr)
source('/vol/milkunB/jbosmans/SolarWindPotentials/Function_evaluate_model.R')

df_PA <- read_xlsx('df_PA_Eur_country_261121.xlsx')
print('NOTE: this was used as a first test. Final results are based on INLA modelling')

# based on distributions (see Script_predictor_stats.R): 
# log_transform travel_time and road_dist and grid and slope and elevation
print('NOTE: grid distance now in KM!')
df_PA$road_log   <- log10(df_PA$road_dist + 1)	# shift away from 0 to avoid -inf in log-transformed values
df_PA$travel_log <- log10(df_PA$access + 1)
df_PA$grid_log   <- log10(df_PA$grid_km + 1)
df_PA$slope_log  <- log10(df_PA$slope)
df_PA$elev_log   <- log10(df_PA$elev + abs(min(df_PA$elev))+1)

print('build a glmer on all input, log-transformed travel time, road distance, grid distance and slope')
print('based on vifs, forest is removed from the predictors')

# original data (no scaling)
model_all <- glmer(PA_bool ~ road_log + grid_log + travel_log + slope_log + elev_log + protect + rsds + Agriculture + Short_natural + Wetland + Urban + Bare + Water + (1|SOVEREIGN1), data = df_PA, family = 'binomial'(link = 'logit'))
#~ load('glmer_model_all.rda')
summary(model_all)
save(model_all, file='glmer_model_all.rda')

print('   test statistics for full model')
eval <- evaluate_model(model_all,df_PA)
print(eval)
r.squaredGLMM(model_all)

res_all <- residuals(model_all)    #obs minus fitted

# ------------------------------------------------------------------------------------
print('   repeat for scaled inputs ---------------------------------------------')
df_scaled <- df_PA
df_scaled[,c('road_log', 'grid_log','travel_log', 'slope_log', 'elev_log', 'rsds', 'Forest', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water', 'available_area')] <- scale(df_PA[,c('road_log','grid_log','travel_log', 'slope_log', 'elev_log', 'rsds', 'Forest', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water', 'available_area')],center=TRUE,scale=TRUE)

model_scaled <- glmer(PA_bool ~ road_log + grid_log + travel_log + slope_log + elev_log + protect + rsds + Agriculture + Short_natural + Wetland + Urban + Bare + Water + (1|SOVEREIGN1), data = df_scaled, family = 'binomial'(link = 'logit'))
#~ load('glmer_model_all.rda')
summary(model_scaled)
save(model_scaled, file='glmer_model_scaled.rda')

print('   test statistics for scaled model')
eval <- evaluate_model(model_scaled,df_scaled)
print(eval)
r.squaredGLMM(model_scaled)
