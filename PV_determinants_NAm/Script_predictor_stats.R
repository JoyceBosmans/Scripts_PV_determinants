pacman::p_load(readxl, writexl, ggplot2, plotly,sf, caret,dplyr)
source("/vol/milkunB/jbosmans/GLM_PV/HighstatLibV10.R")

# check multicollinearity and vifs as well as distribution (normal?) of predictors 

# dataframe is created in Script_PA_predictors.R
df_PA <- read_xlsx('df_PA_NAm_country_060622.xlsx')

# all predictors are numeric except protected status, which is set to true or false
sapply(df_PA,mode)
df_PA$protect_logical <- df_PA$protect > 0

# correlation matrix and vifs ------------------------------------------------------ 
print('NOTE: using grid distance in km instead of degree!!')
colnames(df_PA)[colnames(df_PA) == 'grid_km'] <- 'grid_proximity'
colnames(df_PA)[colnames(df_PA) == 'access']  <- 'travel_time'
colnames(df_PA)[colnames(df_PA) == 'rsds']    <- 'irradiance'
cm <- cor(df_PA[,c("road_dist","grid_proximity","travel_time", "slope", "elev", "protect_logical", "irradiance", "Agriculture", "Forest", "Short_natural", "Wetland", "Urban", "Bare", "Water" )])

pdf(file='Plot_correlation.pdf')
corrplot::corrplot(cm, method = 'number', type = 'lower', number.cex = 0.8)
dev.off()
corvif(df_PA[,c("road_dist","grid_proximity","travel_time", "slope", "elev", "protect_logical", "irradiance", "Agriculture", "Forest", "Short_natural", "Wetland", "Urban", "Bare", "Water" )])	

# ===> exclude forest and travel time (which has a 0.87 correlation with road-dist and gives vif > 5)
# ===> update june 22: exclude forest and road distance. THen R < 0.8 and vif < 5

# check distribution of predictors (first shift those with negative values so log-transforming does not produce NAs)
df_PA$elev_shift       <- df_PA$elev + abs(min(df_PA$elev))+1
df_pred                <- subset(df_PA,select=c('road_dist','slope','elev_shift'))
df_pred$slope_log      <- log10(df_pred$slope)
df_pred$road_dist_log  <- log10(df_pred$road_dist+1)
df_pred$elev_log       <- log10(df_pred$elev_shift)

df_pred2               <- subset(df_PA,select=c('grid_proximity','travel_time'))
df_pred2$grid_log      <- log10(df_pred2$grid_proximity+1)
df_pred2$travel_log    <- log10(df_pred2$travel_time+1)

irr_log <- log10(df_PA$irradiance)

pdf(file='Plot_distribution_1.pdf',width=12,height=14)
par(mfrow=c(6,2),mar=c(2,3,1,1))
hist(df_pred$elev_shift,breaks=100,main="Elevation")
hist(df_pred$elev_log,breaks=100,main="Log Elevation")
hist(df_pred$road_dist,breaks=100,main="Road distance")
hist(df_pred$road_dist_log,breaks=100,main="Log Road distance")
hist(df_pred$slope,breaks=100,main="Slope")
hist(df_pred$slope_log,breaks=100,main="Log Slope")
hist(df_pred2$grid_proximity,breaks=100,main="Grid proximity")
hist(df_pred2$grid_log,breaks=100,main="Log grid proximity")
hist(df_pred2$travel_time,breaks=100,main="Travel time")
hist(df_pred2$travel_log,breaks=100,main="Log Travel time")
hist(df_PA$irradiance,breaks=100,main="Irradiance")
hist(irr_log,breaks=100,main="Log Irradiance")
dev.off()

# --> log_transform travel_time and road_dist and grid and slope. 
# --> also elevation (consistent with other continents)? 

df_ESA <- subset(df_PA,select=c('Agriculture','Forest','Short_natural','Wetland','Urban','Bare','Water'))
# ---> not log-transformed. distribution not normal (lots of 0 values, or also 1 for agriculture and forest) but log-transformed distribution not normal either. 

pdf(file='Plot_distribution_2.pdf',width=12,height=14)
par(mfrow=c(4,2),mar=c(2,3,1,1))
hist(df_ESA$Agriculture,main="Agriculture")
hist(df_ESA$Forest,main="Forest")
hist(df_ESA$Short_natural,main="Short natural")
hist(df_ESA$Wetland,main="Wetland")
hist(df_ESA$Urban,main="Urban")
hist(df_ESA$Bare,main="Bare")
hist(df_ESA$Water,main="Water")
dev.off()
