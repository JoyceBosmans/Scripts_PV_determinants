pacman::p_load(lme4,MuMIn,readxl,dplyr,data.table)
source('/vol/milkunB/jbosmans/SolarWindPotentials/Function_evaluate_model.R')

df_PA          <- read_xlsx('df_PA_Asia_country_261121.xlsx')
outputfilename <- 'df_PI_Asia.Rda'
outputfilename_csv <- 'df_PI_Asia.csv'

# based on distributions (see Script_predictor_stats.R), log-transform road-dist and travel-time
df_PA$road_log   <- log10(df_PA$road_dist + 1)	# shift away from 0 to avoid -inf in log-transformed values
df_PA$travel_log <- log10(df_PA$access + 1)
df_PA$grid_log   <- log10(df_PA$grid_km + 1)
df_PA$slope_log  <- log10(df_PA$slope)
df_PA$elev_log   <- log10(df_PA$elev + abs(min(df_PA$elev))+1)

df_scaled <- df_PA
df_scaled[,c('road_log', 'grid_log','travel_log', 'slope_log', 'elev_log', 'rsds', 'Forest', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water', 'available_area')] <- scale(df_PA[,c('road_log','grid_log','travel_log', 'slope_log', 'elev_log', 'rsds', 'Forest', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water', 'available_area')],center=TRUE,scale=TRUE)

# best model created in Script_find_best_glmer.R: 
load('avg_model.rda')
print(avg_model)

coeffs <- avg_model$coefficients[1,]

coeffs <- as.data.frame(coeffs)
coefft <- transpose(coeffs)
colnames(coefft) <- rownames(coeffs)

best_prediction   <- predict(avg_model, newdata = NULL, type = "response",allow.new.levels=TRUE,full=TRUE)

# create dataframe to add PI (predictor importance, 1 - Spearman R) to. 
df_PI <- data.frame(0,0,0)	#delete this first row later
names(df_PI) <- c('predictor','PI','sign')

# randomize each predictor and determine R
df_random <- transform(df_scaled, elev_log = sample(elev_log,replace=FALSE))
random_prediction <- predict(avg_model, newdata = df_random, type = "response",allow.new.levels=TRUE)
PI                <- 1 - cor(best_prediction,random_prediction,method='spearman')
sign              <- if(coefft$elev[1] < 0) '#cc3300' else '#3399FF'
df_PI_add         <- data.frame('Elevation',PI,sign)
names(df_PI_add)  <- c('predictor','PI','sign')
df_PI             <- rbind(df_PI,df_PI_add)

df_random <- transform(df_scaled, grid_log = sample(grid_log,replace=FALSE))
random_prediction <- predict(avg_model, newdata = df_random, type = "response",allow.new.levels=TRUE)
PI                <- 1 - cor(best_prediction,random_prediction,method='spearman')
sign              <- if(coefft$grid_log[1] < 0) '#cc3300' else '#3399FF'
df_PI_add         <- data.frame('Grid distance',PI,sign)
names(df_PI_add)  <- c('predictor','PI','sign')
df_PI             <- rbind(df_PI,df_PI_add)

df_random <- transform(df_scaled, rsds = sample(rsds,replace=FALSE))
random_prediction <- predict(avg_model, newdata = df_random, type = "response",allow.new.levels=TRUE)
PI                <- 1 - cor(best_prediction,random_prediction,method='spearman')
sign              <- if(coefft$rsds[1] < 0) '#cc3300' else '#3399FF'
df_PI_add         <- data.frame('Irradiation',PI,sign)
names(df_PI_add)  <- c('predictor','PI','sign')
df_PI             <- rbind(df_PI,df_PI_add)

df_random <- transform(df_scaled, protect = sample(protect,replace=FALSE))
random_prediction <- predict(avg_model, newdata = df_random, type = "response",allow.new.levels=TRUE)
PI                <- 1 - cor(best_prediction,random_prediction,method='spearman')
sign              <- if(coefft$protect[1] < 0) '#cc3300' else '#3399FF'
df_PI_add         <- data.frame('Protected status',PI,sign)
names(df_PI_add)  <- c('predictor','PI','sign')
df_PI             <- rbind(df_PI,df_PI_add)

df_random <- transform(df_scaled, road_log = sample(road_log,replace=FALSE))
random_prediction <- predict(avg_model, newdata = df_random, type = "response",allow.new.levels=TRUE)
PI                <- 1 - cor(best_prediction,random_prediction,method='spearman')
sign              <- if(coefft$road_log[1] < 0) '#cc3300' else '#3399FF'
df_PI_add         <- data.frame('Road distance',PI,sign)
names(df_PI_add)  <- c('predictor','PI','sign')
df_PI             <- rbind(df_PI,df_PI_add)

df_random <- transform(df_scaled, slope_log = sample(slope_log,replace=FALSE))
random_prediction <- predict(avg_model, newdata = df_random, type = "response",allow.new.levels=TRUE)
PI                <- 1 - cor(best_prediction,random_prediction,method='spearman')
sign              <- if(coefft$slope_log[1] < 0) '#cc3300' else '#3399FF'
df_PI_add         <- data.frame('Slope',PI,sign)
names(df_PI_add)  <- c('predictor','PI','sign')
df_PI             <- rbind(df_PI,df_PI_add)

df_random <- transform(df_scaled, travel_log = sample(travel_log,replace=FALSE))
random_prediction <- predict(avg_model, newdata = df_random, type = "response",allow.new.levels=TRUE)
PI                <- 1 - cor(best_prediction,random_prediction,method='spearman')
sign              <- if(coefft$travel_log[1] < 0) '#cc3300' else '#3399FF'
df_PI_add         <- data.frame('Travel times',PI,sign)
names(df_PI_add)  <- c('predictor','PI','sign')
df_PI             <- rbind(df_PI,df_PI_add)

df_random    <- transform(df_scaled, Agriculture = sample(Agriculture,replace=FALSE))
#model_random <- glmer(PA_bool ~ grid_log + travel_log + slope_log + elev + protect + rsds + Agriculture + Short_natural + Wetland + Bare + Water + (1|SOVEREIGN1), data = df_random, family = 'binomial'(link = 'logit'))
#r.squaredGLMM(model_random)
# which R, pearson or spearman?
# compute by correlating the two predictions with cor(method='spearman')?
random_prediction <- predict(avg_model, newdata = df_random, type = "response",allow.new.levels=TRUE)
PI                <- 1 - cor(best_prediction,random_prediction,method='spearman')
sign              <- if(coefft$Agriculture[1] < 0) '#cc3300' else '#3399FF'
df_PI_add         <- data.frame('Agriculture',PI,sign)
names(df_PI_add)  <- c('predictor','PI','sign')
df_PI             <- rbind(df_PI,df_PI_add)

df_random <- transform(df_scaled, Bare = sample(Bare,replace=FALSE))
random_prediction <- predict(avg_model, newdata = df_random, type = "response",allow.new.levels=TRUE)
PI                <- 1 - cor(best_prediction,random_prediction,method='spearman')
sign              <- if(coefft$Bare[1] < 0) '#cc3300' else '#3399FF'
df_PI_add         <- data.frame('Bare',PI,sign)
names(df_PI_add)  <- c('predictor','PI','sign')
df_PI             <- rbind(df_PI,df_PI_add)

df_random <- transform(df_scaled, Short_natural = sample(Short_natural,replace=FALSE))
random_prediction <- predict(avg_model, newdata = df_random, type = "response",allow.new.levels=TRUE)
PI                <- 1 - cor(best_prediction,random_prediction,method='spearman')
sign              <- if(coefft$Short_natural[1] < 0) '#cc3300' else '#3399FF'
df_PI_add         <- data.frame('Short_natural',PI,sign)
names(df_PI_add)  <- c('predictor','PI','sign')
df_PI             <- rbind(df_PI,df_PI_add)

# urban: not in best model so set to 0
df_PI_add         <- data.frame('Urban',0,0)
names(df_PI_add)  <- c('predictor','PI','sign')
df_PI             <- rbind(df_PI,df_PI_add)

df_random <- transform(df_scaled, Water = sample(Water,replace=FALSE))
random_prediction <- predict(avg_model, newdata = df_random, type = "response",allow.new.levels=TRUE)
PI                <- 1 - cor(best_prediction,random_prediction,method='spearman')
sign              <- if(coefft$Water[1] < 0) '#cc3300' else '#3399FF'
df_PI_add         <- data.frame('Water',PI,sign)
names(df_PI_add)  <- c('predictor','PI','sign')
df_PI             <- rbind(df_PI,df_PI_add)

df_random <- transform(df_scaled, Wetland = sample(Wetland,replace=FALSE))
random_prediction <- predict(avg_model, newdata = df_random, type = "response",allow.new.levels=TRUE)
PI                <- 1 - cor(best_prediction,random_prediction,method='spearman')
sign              <- if(coefft$Wetland[1] < 0) '#cc3300' else '#3399FF'
df_PI_add         <- data.frame('Wetland',PI,sign)
names(df_PI_add)  <- c('predictor','PI','sign')
df_PI             <- rbind(df_PI,df_PI_add)

# delete first row then re-order rows (as barplot puts first row at the bottom)
df_PI <- df_PI[-c(1),]
df_PI <- df_PI[rev(rownames(df_PI)),]
rownames(df_PI) <- NULL

#ggplot(data=df_PI, aes(x=PI,y=predictor)) + geom_bar(stat="identity")
barplot(df_PI$PI,names.arg=df_PI$predictor,horiz=TRUE,las=1,col=df_PI$sign,border=df_PI$sign)
# no y labels: add yaxt= 'n'

save(df_PI,file=outputfilename)
write.csv(df_PI,outputfilename_csv) 
