pacman::p_load(rgdal, readxl, writexl, ggplot2, spData, sp, sf, lme4,MuMIn,dplyr,optimx,dfoptim)
source('/vol/milkunB/jbosmans/SolarWindPotentials/Function_evaluate_model.R')

df_PA <- read_xlsx('df_PA_Asia_country_261121.xlsx')

# based on distributions (see Script_predictor_stats.R), log-transform road-dist and travel-time
df_PA$road_log   <- log10(df_PA$road_dist + 1)	# shift away from 0 to avoid -inf in log-transformed values
df_PA$travel_log <- log10(df_PA$access + 1)
df_PA$grid_log   <- log10(df_PA$grid_km + 1)
df_PA$slope_log  <- log10(df_PA$slope)
df_PA$elev_log   <- log10(df_PA$elev + abs(min(df_PA$elev))+1)

df_scaled <- df_PA
df_scaled[,c('road_log', 'grid_log','travel_log', 'slope_log', 'elev_log', 'rsds', 'Forest', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water', 'available_area')] <- scale(df_PA[,c('road_log','grid_log','travel_log', 'slope_log', 'elev_log', 'rsds', 'Forest', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water', 'available_area')],center=TRUE,scale=TRUE)

# original data (no scaling)
load('glmer_model_all.rda')
#~ summary(model_all)
#~ save(model_all, file='glmer_model_all.rda')

# best model:
load('topmodel.rda')
model_all <- topmodel

#~ # avg model
#~ load('avg_model.rda')
#~ model_all <- avg_model

# optimizer bobyqa WORKS
ss           <- getME(model_all,c("theta","fixef"))
model_bobyqa <- update(model_all,start=ss,control=glmerControl(optimizer="bobyqa"))

# optimizer nlminbwrap WORKS
model_nlminbwrap <- update(model_all,start=ss,control=glmerControl(optimizer="nlminbwrap"))

# optimizer="optimx",optCtrl=list(method="L-BFGS-B") WORKS
model_optimx <- update(model_all,start=ss,control=glmerControl(optimizer="optimx",optCtrl=list(method="L-BFGS-B")))

# optimizer="nloptwrap",optCtrl=list(method="NLOPT_LN_NELDERMEAD") WORKS
model_nloptwrap <- update(model_all,start=ss,control=glmerControl(optimizer="nloptwrap",optCtrl=list(method="NLOPT_LN_NELDERMEAD")))

# optimizer="nloptwrap",optCtrl=list(method="NLOPT_LN_BOBYQA") WORKS
model_nloptwrap2 <- update(model_all,start=ss,control=glmerControl(optimizer="nloptwrap",optCtrl=list(method="NLOPT_LN_BOBYQA")))

# df of coefficients of (working) optimizers
coeff_neldermead <- data.frame(state=names(fixef(model_all)),neldermead=as.vector(fixef(model_all)))
coeff_nlminbwrap <- data.frame(state=names(fixef(model_nlminbwrap)),nlminbwrap=as.vector(fixef(model_all)))
coeff_optimx     <- data.frame(state=names(fixef(model_optimx)),optimx=as.vector(fixef(model_all)))
coeff_bobyqa     <- data.frame(state=names(fixef(model_bobyqa)),bobyqa=as.vector(fixef(model_bobyqa)))
coeff_nloptwrap  <- data.frame(state=names(fixef(model_nloptwrap)),nloptwrap=as.vector(fixef(model_nloptwrap)))
coeff_nloptwrap2 <- data.frame(state=names(fixef(model_nloptwrap2)),nloptwrap2=as.vector(fixef(model_nloptwrap2)))

coeff <- coeff_bobyqa
coeff$neldermead <- coeff_neldermead$neldermead
coeff$nlminbwrap <- coeff_nlminbwrap$nlminbwrap
coeff$optimx     <- coeff_optimx$optimx
coeff$nloptwrap  <- coeff_nloptwrap$nloptwrap
coeff$nloptwrap2 <- coeff_nloptwrap2$nloptwrap2

coeff

print('Conclusion: different optimizers yield the same results. So ignore warnings.')

