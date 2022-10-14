pacman::p_load(readxl, writexl, ggplot2, plotly,sf, caret,dplyr)
source("/vol/milkunB/jbosmans/GLM_PV/HighstatLibV10.R")

# check multicollinearity and vifs as well as distribution (normal?) of predictors 

# dataframe is created in Script_PA_predictors.R
df_PA <- read_xlsx('df_PA_global_country.xlsx')

# all predictors are numeric except protected status, which is set to true or false
sapply(df_PA,mode)
df_PA$protect_logical <- df_PA$protect > 0

# correlation matrix and vifs ------------------------------------------------------ 
colnames(df_PA)[colnames(df_PA) == 'grid'] <- 'grid_proximity'
colnames(df_PA)[colnames(df_PA) == 'access'] <- 'travel_time'
colnames(df_PA)[colnames(df_PA) == 'rsds'] <- 'irradiance'
cm <- cor(df_PA[,c("road_dist","urban_dist","grid_proximity","travel_time", "slope", "elev", "protect_logical", "irradiance", "Agriculture", "Forest", "Short_natural", "Wetland", "Urban", "Bare", "Water" )])

pdf(file='Plot_correlation.pdf')
corrplot::corrplot(cm, method = 'number', type = 'lower', number.cex = 0.8)
dev.off()
corvif(df_PA[,c("road_dist","urban_dist","grid_proximity","travel_time", "slope", "elev", "protect_logical", "irradiance", "Agriculture", "Forest", "Short_natural", "Wetland", "Urban", "Bare", "Water" )])	

#Warning message:
#In cov2cor(v) : diag(.) had 0 or NA entries; non-finite result is doubtful
#https://groups.google.com/g/lavaan/c/9xDGCK7fF5g -> no NAs on diagonal of cor.matrix, what about cov.matrix?

cmv <- cov(df_PA[,c("road_dist","urban_dist","grid_proximity","travel_time", "slope", "elev", "protect_logical", "irradiance", "Agriculture", "Forest", "Short_natural", "Wetland", "Urban", "Bare", "Water" )])
# ==> no 0 or NA entries. also no 0 or NA standard deviations

#if any land cover is removed, it works. Related to the additive nature of land cover fractions?
#vifs don't change much whether only Agriculture, Forest or Short natural is removed
#vifs are too high if only wetland, urban or water is removed. Removing Bare results in vifs < 5 but slightly higher than removing any of the first 3. 
#removing wetland: Agri 20.74, For 20.98, Sho 19.50
#removing urban:   agri 57.76, For 62.20, Sho 58.08
#removing water:   agri 20.89, For 21.04, Sho 19.64 => vif for forest thus highest in each case
#based on correlations, remove either forest or agriculture (highest cor)

#fitting a glm works fine, no error messages. glmer worked but with fit warnings (failed to converge, nearly unidentifiable: very large eigenvalue (ratio))
# modeltest <- glmer(PA_bool ~ road_dist + urban_dist + grid_proximity + travel_time + slope + elev + protect_logical + irradiance + Forest + Short_natural + Wetland + Urban + Bare + Water + (1|SOVEREIGN1), data = df_PA)

#############


# check distribution of predictors (first shift those with negative values so log-transforming does not produce NAs)
df_PA$elev_shift       <- df_PA$elev + abs(min(df_PA$elev))
df_PA$urban_dist_shift <- df_PA$urban_dist + abs(min(df_PA$urban_dist))
df_pred                <- subset(df_PA,select=c('road_dist','urban_dist_shift','slope','elev_shift'))
df_pred$slope_log      <- log10(df_pred$slope)
df_pred$road_dist_log  <- log10(df_pred$road_dist)
df_pred$elev_log       <- log10(df_pred$elev_shift)
df_pred$urban_dist_log <- log10(df_pred$urban_dist_shift)
#~ pdf(file='Plot_distribution_1.pdf',width=14,height=8)
#~ df_pred %>% select_if(is.numeric) %>%  tidyr::gather(cols, value) %>%  ggplot(aes(x = value)) + geom_histogram(bins=100) + facet_grid(.~cols)
#~ dev.off()

df_pred2            <- subset(df_PA,select=c('grid_proximity','travel_time'))
df_pred2$grid_log   <- log10(df_pred2$grid_proximity)
df_pred2$travel_log <- log10(df_pred2$travel_time)

irr_log <- log10(df_PA$irradiance)

dev.off()
pdf(file='Plot_distribution_1.pdf',width=12,height=14)
par(mfrow=c(7,2),mar=c(2,3,1,1))
hist(df_pred$elev_shift,breaks=100,main="Elevation")
hist(df_pred$elev_log,breaks=100,main="Log Elevation")
hist(df_pred$road_dist,breaks=100,main="Road distance")
hist(df_pred$road_dist_log,breaks=100,main="Log Road distance")
hist(df_pred$urban_dist_shift,breaks=100,main="Urban distance")
hist(df_pred$urban_dist_log,breaks=100,main="Log Urban distance")
hist(df_pred$slope,breaks=100,main="Slope")
hist(df_pred$slope_log,breaks=100,main="Log Slope")
hist(df_pred2$grid_proximity,breaks=100,main="Grid proximity")
hist(df_pred2$grid_log,breaks=100,main="Log grid proximity")
hist(df_pred2$travel_time,breaks=100,main="Travel time")
hist(df_pred2$travel_log,breaks=100,main="Log Travel time")
hist(df_PA$irradiance,breaks=100,main="Irradiance")
hist(irr_log,breaks=100,main="Log Irradiance")
dev.off()

# --> log_transform travel_time and road_dist

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

# TODO: check distributions of P and A separately?
