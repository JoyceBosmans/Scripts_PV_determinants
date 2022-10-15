pacman::p_load(readxl,ggplot2,RColorBrewer,ape,DHARMa)

df_PA <- read_xlsx('df_PA_Eur_country_261121.xlsx')

df_PA$road_log   <- log10(df_PA$road_dist + 1)	# shift away from 0 to avoid -inf in log-transformed values
df_PA$travel_log <- log10(df_PA$access + 1)
df_PA$grid_log   <- log10(df_PA$grid_km + 1)
df_PA$slope_log  <- log10(df_PA$slope)
df_PA$elev_log   <- log10(df_PA$elev + abs(min(df_PA$elev))+1)

df_scaled <- df_PA
df_scaled[,c('road_log', 'grid_log','travel_log', 'slope_log', 'elev_log', 'rsds', 'Forest', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water', 'available_area')] <- scale(df_PA[,c('road_log','grid_log','travel_log', 'slope_log', 'elev_log', 'rsds', 'Forest', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water', 'available_area')],center=TRUE,scale=TRUE)

# model with all predictors, created in Script_glmer.R: 
print('NOTE: using model based on scaled predictors!')
load('glmer_model_scaled.rda')
summary(model_scaled)

df_PA$res <- residuals(model_scaled)    #obs minus fitted

# check distribution of residuals
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(-2.5,2.5))

pdf(file='Plot_residuals.pdf')
ggplot() + theme_minimal() + geom_point(data=df_PA,aes(x=lon_orig,y=lat_orig,color=res),size=0.5) + sc
dev.off()

# Moran's I needs weights -> inverse distance. Matrix? (so distances to all other points?)
distances <- as.matrix(dist(cbind(df_PA$lon_orig, df_PA$lat_orig)))		# what is the unit??
weights   <- 1/distances
weights[is.infinite(weights)] <- 0

test <- distm(df_PA$lon_orig, df_PA$lat_orig,distGeo)	# check that this is correct way to compute distances on sphere

#~ df_PA$PA_num <- 0
#~ df_PA$PA_num[df_PA$PA == 'P'] <- 1
#~ moranI    <- Moran.I(df_PA$PA_num,weights)		
#~ moranI

# DHARMa:
simulationOutput <- simulateResiduals(fittedModel = model_scaled, plot = F)
df_PA$res_dharma <- residuals(simulationOutput)
moranI    <- Moran.I(df_PA$res_dharma,weights)		
moranI


## package Konrad: https://cran.r-project.org/web/packages/spaMM/index.html
