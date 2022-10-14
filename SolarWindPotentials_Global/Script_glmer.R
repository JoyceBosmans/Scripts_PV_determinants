pacman::p_load(rgdal, readxl, writexl, ggplot2, spData, sp, sf, lme4,MuMIn)
source('/vol/milkunB/jbosmans/SolarWindPotentials/Function_evaluate_model.R')

df_PA <- read_xlsx('df_PA_global_country.xlsx')

# based on distributions (see Script_predictor_stats.R), log-transform road-dist and travel-time
df_PA$road_log   <- log10(df_PA$road_dist + 1)	# shift away from 0 to avoid -inf in log-transformed values
df_PA$travel_log <- log10(df_PA$access + 1)

print('build a glmer on all input, log-transformed travel time and road distance')
print('based on vifs, forest is removed from the predictors')
model_all <- glmer(PA_bool ~ road_log + urban_dist + grid + travel_log + slope + elev + protect + rsds + Agriculture + Short_natural + Wetland + Urban + Bare + Water + (1|SOVEREIGN1), data = df_PA, family = 'binomial'(link = 'logit'))
#~ load('glmer_model_all.rda')
summary(model_all)
save(model_all, file='glmer_model_all.rda')

print('   test statistics for full model')
eval <- evaluate_model(model_all,df_PA)
print(eval)
r.squaredGLMM(model_all)

res_all <- residuals(model_all)    #obs minus fitted

######################################################################################
print('   repeat for scaled inputs ---------------------------------------------')
df_scaled <- df_PA
df_scaled[,c('road_log', 'urban_dist', 'grid','travel_log', 'slope', 'elev', 'rsds', 'Forest', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water', 'available_area')] <- scale(df_PA[,c('road_log', 'urban_dist', 'grid','travel_log', 'slope', 'elev', 'rsds', 'Forest', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water', 'available_area')],center=TRUE,scale=TRUE)

model_all_scaled <- glmer(PA_bool ~ road_log + urban_dist + grid + travel_log + slope + elev + protect + rsds + Agriculture + Short_natural + Wetland + Urban + Bare + Water + (1|SOVEREIGN1), data = df_scaled, family = 'binomial'(link = 'logit'))
#~ load('glmer_model_all_scaled.rda')
summary(model_all_scaled)
save(model_all_scaled, file='glmer_model_all_scaled.rda')

print('   test statistics for full scaled model')
eval <- evaluate_model(model_all_scaled,df_scaled)
print(eval)
r.squaredGLMM(model_all_scaled)

res_all_scaled <- residuals(model_all_scaled) 

#~ pdf(file='Plot_residuals_all.pdf',width=10,height=8)
#~ par(mfrow=c(2,1),mar=c(2,3,1,1))
#~ hist(res_all,breaks=100,main="Residuals of full model")
#~ hist(res_all_scaled,breaks=100,main="Same but scaled inputs")
#~ dev.off()

#~ plot(df_PA$PA_bool,res_all_scaled) #same for res_all
#residues all negative for absences, all positive for presences
#this is a consequence of observed absences set to 0, observed presences to 1. 
#https://www.theanalysisfactor.com/what-is-logit-function/

####################################################################################
print('    repeat with country nested in continent -------------------------------')
model_continent <- glmer(PA_bool ~ road_log + urban_dist + grid + travel_log + slope + elev + protect + rsds + Agriculture + Short_natural + Wetland + Urban + Bare + Water + (1|continent/SOVEREIGN1), data = df_PA, family = 'binomial'(link = 'logit'))
#load('glmer_model_continent.rda')
summary(model_continent)
save(model_continent, file='glmer_model_continent.rda')

print('   test statistics for full model with country nested in continent')
eval <- evaluate_model(model_continent,df_PA)
print(eval)
r.squaredGLMM(model_continent)

res_all <- residuals(model_continent)    #obs minus fitted

print('   and nested with scaled inputs ------------------------------------------')
model_scaled_continent <- glmer(PA_bool ~ road_log + urban_dist + grid + travel_log + slope + elev + protect + rsds + Agriculture + Short_natural + Wetland + Urban + Bare + Water + (1|continent/SOVEREIGN1), data = df_scaled, family = 'binomial'(link = 'logit'))
#load('glmer_model_scaled_continent.rda')
summary(model_scaled_continent)
save(model_scaled_continent, file='glmer_model_scaled_continent.rda')

print('   test statistics for full model with country nested in continent')
eval <- evaluate_model(model_scaled_continent,df_PA)
print(eval)
r.squaredGLMM(model_scaled_continent)

res_all <- residuals(model_scaled_continent)    #obs minus fitted
