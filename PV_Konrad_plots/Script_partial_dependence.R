pacman::p_load(ggplot2, writexl,data.table,readxl,ggpubr)

dir_Konrad <- '/vol/milkunarc2/kmielke/pv' 

# load df_PA (data frame), in which predictors are already log-transformed and standardized
load(file = file.path(dir_Konrad,'/data/rda/df_PA_Eur_country_060622_proprocessed.Rda'))
df_PA_Eur  <- df_PA
load(file = file.path(dir_Konrad,'/data/rda/df_PA_NAm_country_060622_proprocessed.Rda'))
df_PA_NAm  <- df_PA
load(file = file.path(dir_Konrad,'/data/rda/df_PA_SAm_country_060622_proprocessed.Rda'))
df_PA_SAm  <- df_PA
load(file = file.path(dir_Konrad,'/data/rda/df_PA_Asia_country_060622_proprocessed.Rda'))
df_PA_Asia <- df_PA
load(file = file.path(dir_Konrad,'/data/rda/df_PA_Afr_country_proprocessed.Rda'))
df_PA_Afr  <- df_PA
load(file = file.path(dir_Konrad,'/data/rda/df_PA_Oc_country_proprocessed.Rda'))
df_PA_Oc   <- df_PA

df_PA_Eur_orig  <- read_xlsx('../PV_determinants_Eur/df_PA_Eur_country_060622.xlsx')
df_PA_NAm_orig  <- read_xlsx('../PV_determinants_NAm/df_PA_NAm_country_060622.xlsx')
df_PA_SAm_orig  <- read_xlsx('../PV_determinants_SAm/df_PA_SAm_country_060622.xlsx')
df_PA_Asia_orig <- read_xlsx('../PV_determinants_Asia/df_PA_Asia_country_060622.xlsx')
df_PA_Afr_orig  <- read_xlsx('../PV_determinants_Afr/df_PA_Afr_country.xlsx')
df_PA_Oc_orig   <- read_xlsx('../PV_determinants_Oc/df_PA_Oc_country.xlsx')

# predictor importance as computed in Script_predictor_importance.R
load('../PV_Konrad_Eur/tab_coef_Eur.Rda')
load('../PV_Konrad_NAm/tab_coef_NAm.Rda')
load('../PV_Konrad_SAm/tab_coef_SAm.Rda')
load('../PV_Konrad_Asia/tab_coef_Asia.Rda')
load('../PV_Konrad_Afr/tab_coef_Afr.Rda')
load('../PV_Konrad_Oc/tab_coef_Oc.Rda')

# since we worked with normalized data, all mean values are 0. So applying the model for partial dependency plots is simly only including the relevant predictor

theme_settings <-  list(theme_bw(),theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank()), theme(panel.border= element_blank()), theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5)),theme(axis.text=element_text(size=13),axis.title=element_text(size=14),plot.title = element_text(size=18)), ylim(0,0.8))


### Europe -----------------------------------------------------------------------------------
predict_rsds     <- tab_coef_Eur['intercept',] + tab_coef_Eur['rsds',]*df_PA_Eur$rsds
probability_rsds <- 1/(1 + exp(-predict_rsds))
df_PA_Eur_orig$predict_rsds <- probability_rsds

predict_travel     <- tab_coef_Eur['intercept',] + tab_coef_Eur['travel_log',]*df_PA_Eur$travel_log
probability_travel <- 1/(1 + exp(-predict_travel))
df_PA_Eur_orig$predict_travel <- probability_travel
df_PA_Eur_orig$travel_times   <- df_PA_Eur_orig$access / 60	# travel times in hours

#~ predict_Agriculture_Eur     <- tab_coef_Eur['intercept',] + tab_coef_Eur['Agriculture',]*df_PA_Eur$Agriculture
#~ probability_Agriculture_Eur <- 1/(1 + exp(-predict_Agriculture_Eur))
#~ quantile(probability_Agriculture_Eur)
#~ plot(df_PA_Eur_orig$Agriculture,probability_Agriculture_Eur)

predict_Short_natural     <- tab_coef_Eur['intercept',] + tab_coef_Eur['Short_natural',]*df_PA_Eur$Short_natural
probability_Short_natural <- 1/(1 + exp(-predict_Short_natural))
df_PA_Eur_orig$predict_short <- probability_Short_natural

# set up plots
Eur_rsds   <- ggplot(df_PA_Eur_orig,aes(x=rsds,y=predict_rsds)) + geom_point() + theme_settings + ylab('Europe') + xlab('') + scale_y_continuous(limits=c(0,0.8)) + scale_x_continuous(limits = c(500,2750))

Eur_travel <- ggplot(df_PA_Eur_orig,aes(x=travel_times,y=predict_travel)) + geom_point() + theme_settings+ ylab('') + xlab('') + scale_y_continuous(limits=c(0,0.8)) + scale_x_continuous(limits = c(0,80))

empty      <- data.frame()	# grid distance was not included in model for Eur
Eur_grid   <- ggplot(empty) + geom_point() + theme_minimal() + theme(plot.title=element_text(size=18))

Eur_short  <- ggplot(df_PA_Eur_orig,aes(x=Short_natural,y=predict_short)) + geom_point() + theme_settings+ ylab('') + xlab('') + scale_y_continuous(limits=c(0,0.8))

### North America -----------------------------------------------------------------------------------
predict_rsds     <- tab_coef_NAm['intercept',] + tab_coef_NAm['rsds',]*df_PA_NAm$rsds
probability_rsds <- 1/(1 + exp(-predict_rsds))
df_PA_NAm_orig$predict_rsds <- probability_rsds

predict_travel     <- tab_coef_NAm['intercept',] + tab_coef_NAm['travel_log',]*df_PA_NAm$travel_log
probability_travel <- 1/(1 + exp(-predict_travel))
df_PA_NAm_orig$predict_travel <- probability_travel
df_PA_NAm_orig$travel_times   <- df_PA_NAm_orig$access / 60	# travel times in hours

predict_grid       <- tab_coef_NAm['intercept',] + tab_coef_NAm['grid_log',]*df_PA_NAm$grid_log
probability_grid   <- 1/(1 + exp(-predict_grid))
df_PA_NAm_orig$predict_grid <- probability_grid
df_PA_NAm_orig$grid_km      <- df_PA_NAm_orig$grid_km/1000	# m to km

predict_Short_natural     <- tab_coef_NAm['intercept',] + tab_coef_NAm['Short_natural',]*df_PA_NAm$Short_natural
probability_Short_natural <- 1/(1 + exp(-predict_Short_natural))
df_PA_NAm_orig$predict_short <- probability_Short_natural

# set up plots
NAm_rsds   <- ggplot(df_PA_NAm_orig,aes(x=rsds,y=predict_rsds)) + geom_point() + theme_settings + ylab('North America') + xlab('') + scale_y_continuous(limits=c(0,0.8)) + ggtitle('Irradiation') + scale_x_continuous(limits = c(500,2750))

NAm_travel <- ggplot(df_PA_NAm_orig,aes(x=travel_times,y=predict_travel)) + geom_point() + theme_settings+ ylab('') + xlab('') + scale_y_continuous(limits=c(0,0.8)) + ggtitle('Travel times') + scale_x_continuous(limits = c(0,80))

NAm_grid   <- ggplot(df_PA_NAm_orig,aes(x=grid_km,y=predict_grid)) + geom_point() + theme_settings+ ylab('') + xlab('') + scale_y_continuous(limits=c(0,0.8)) + scale_x_continuous(limits=c(0,1000)) + ggtitle('Grid distance') # excludes 1134 locations with larger distance (~4.5%) +

NAm_short  <- ggplot(df_PA_NAm_orig,aes(x=Short_natural,y=predict_short)) + geom_point() + theme_settings+ ylab('') + xlab('') + scale_y_continuous(limits=c(0,0.8)) + ggtitle('Short natural')


### South America -----------------------------------------------------------------------------------
predict_rsds     <- tab_coef_SAm['intercept',] + tab_coef_SAm['rsds',]*df_PA_SAm$rsds
probability_rsds <- 1/(1 + exp(-predict_rsds))
df_PA_SAm_orig$predict_rsds <- probability_rsds

predict_travel     <- tab_coef_SAm['intercept',] + tab_coef_SAm['travel_log',]*df_PA_SAm$travel_log
probability_travel <- 1/(1 + exp(-predict_travel))
df_PA_SAm_orig$predict_travel <- probability_travel
df_PA_SAm_orig$travel_times   <- df_PA_SAm_orig$access / 60	# travel times in hours

predict_grid       <- tab_coef_SAm['intercept',] + tab_coef_SAm['grid_log',]*df_PA_SAm$grid_log
probability_grid   <- 1/(1 + exp(-predict_grid))
df_PA_SAm_orig$predict_grid <- probability_grid
df_PA_SAm_orig$grid_km      <- df_PA_SAm_orig$grid_km/1000	# m to km

# set up plots
SAm_rsds   <- ggplot(df_PA_SAm_orig,aes(x=rsds,y=predict_rsds)) + geom_point() + theme_settings + ylab('South America') + xlab('') + scale_y_continuous(limits=c(0,0.8))  + scale_x_continuous(limits = c(500,2750))

SAm_travel <- ggplot(df_PA_SAm_orig,aes(x=travel_times,y=predict_travel)) + geom_point() + theme_settings+ ylab('') + xlab('') + scale_y_continuous(limits=c(0,0.8)) + scale_x_continuous(limits = c(0,80))

SAm_grid   <- ggplot(df_PA_SAm_orig,aes(x=grid_km,y=predict_grid)) + geom_point() + theme_settings+ ylab('') + xlab('') + scale_y_continuous(limits=c(0,0.8)) + scale_x_continuous(limits=c(0,1000))	# excludes 2 locations with larger distance 

empty      <- data.frame()	# short natural was not included in model for SAm
SAm_short  <- ggplot(empty) + geom_point() + theme_minimal() + theme(plot.title=element_text(size=18))


### Asia -----------------------------------------------------------------------------------
predict_rsds     <- tab_coef_Asia['intercept',] + tab_coef_Asia['rsds',]*df_PA_Asia$rsds
probability_rsds <- 1/(1 + exp(-predict_rsds))
df_PA_Asia_orig$predict_rsds <- probability_rsds

predict_travel     <- tab_coef_Asia['intercept',] + tab_coef_Asia['travel_log',]*df_PA_Asia$travel_log
probability_travel <- 1/(1 + exp(-predict_travel))
df_PA_Asia_orig$predict_travel <- probability_travel
df_PA_Asia_orig$travel_times   <- df_PA_Asia_orig$access / 60	# travel times in hours

predict_grid       <- tab_coef_Asia['intercept',] + tab_coef_Asia['grid_log',]*df_PA_Asia$grid_log
probability_grid   <- 1/(1 + exp(-predict_grid))
df_PA_Asia_orig$predict_grid <- probability_grid
df_PA_Asia_orig$grid_km      <- df_PA_Asia_orig$grid_km/1000	# m to km

predict_Short_natural     <- tab_coef_Asia['intercept',] + tab_coef_Asia['Short_natural',]*df_PA_Asia$Short_natural
probability_Short_natural <- 1/(1 + exp(-predict_Short_natural))
df_PA_Asia_orig$predict_short <- probability_Short_natural

# set up plots
Asia_rsds   <- ggplot(df_PA_Asia_orig,aes(x=rsds,y=predict_rsds)) + geom_point() + theme_settings + ylab('Asia') + xlab('') + scale_y_continuous(limits=c(0,0.3)) + xlab('Irradiance (kWh/m2/yr)') + scale_x_continuous(limits = c(500,2750))

Asia_travel <- ggplot(df_PA_Asia_orig,aes(x=travel_times,y=predict_travel)) + geom_point() + theme_settings+ ylab('') + xlab('') + scale_y_continuous(limits=c(0,0.3)) + xlab('Travel times (hours)') + scale_x_continuous(limits = c(0,80))

Asia_grid   <- ggplot(df_PA_Asia_orig,aes(x=grid_km,y=predict_grid)) + geom_point() + theme_settings+ ylab('') + xlab('') + scale_y_continuous(limits=c(0,0.3)) + scale_x_continuous(limits=c(0,1000)) + xlab('Grid distance (km)') # excludes 13 

Asia_short  <- ggplot(df_PA_Asia_orig,aes(x=Short_natural,y=predict_short)) + geom_point() + theme_settings+ ylab('') + xlab('') + scale_y_continuous(limits=c(0,0.3)) + xlab('Short natural fraction')

### Afr -----------------------------------------------------------------------------------
predict_rsds     <- tab_coef_Afr['intercept',] + tab_coef_Afr['rsds',]*df_PA_Afr$rsds
probability_rsds <- 1/(1 + exp(-predict_rsds))
df_PA_Afr_orig$predict_rsds <- probability_rsds

predict_travel     <- tab_coef_Afr['intercept',] + tab_coef_Afr['travel_log',]*df_PA_Afr$travel_log
probability_travel <- 1/(1 + exp(-predict_travel))
df_PA_Afr_orig$predict_travel <- probability_travel
df_PA_Afr_orig$travel_times   <- df_PA_Afr_orig$access / 60	# travel times in hours

predict_grid       <- tab_coef_Afr['intercept',] + tab_coef_Afr['grid_log',]*df_PA_Afr$grid_log
probability_grid   <- 1/(1 + exp(-predict_grid))
df_PA_Afr_orig$predict_grid <- probability_grid
df_PA_Afr_orig$grid_km      <- df_PA_Afr_orig$grid_km/1000	# m to km

predict_Short_natural     <- tab_coef_Afr['intercept',] + tab_coef_Afr['Short_natural',]*df_PA_Afr$Short_natural
probability_Short_natural <- 1/(1 + exp(-predict_Short_natural))
df_PA_Afr_orig$predict_short <- probability_Short_natural

# set up plots
Afr_rsds   <- ggplot(df_PA_Afr_orig,aes(x=rsds,y=predict_rsds)) + geom_point() + theme_settings + ylab('Africa') + xlab('') + scale_y_continuous(limits=c(0,0.3)) + xlab('') + scale_x_continuous(limits = c(500,2750))

Afr_travel <- ggplot(df_PA_Afr_orig,aes(x=travel_times,y=predict_travel)) + geom_point() + theme_settings+ ylab('') + xlab('') + scale_y_continuous(limits=c(0,0.3)) + xlab('') + scale_x_continuous(limits = c(0,80))

Afr_grid   <- ggplot(df_PA_Afr_orig,aes(x=grid_km,y=predict_grid)) + geom_point() + theme_settings+ ylab('') + xlab('') + scale_y_continuous(limits=c(0,0.3)) + scale_x_continuous(limits=c(0,1000)) + xlab('') # excludes 13 

Afr_short  <- ggplot(df_PA_Afr_orig,aes(x=Short_natural,y=predict_short)) + geom_point() + theme_settings+ ylab('') + xlab('') + scale_y_continuous(limits=c(0,0.3)) + xlab('')

#Oceania--------------------------------------------------------------------------------------------------
predict_travel     <- tab_coef_Oc['intercept',] + tab_coef_Oc['travel_log',]*df_PA_Oc$travel_log
probability_travel <- 1/(1 + exp(-predict_travel))
df_PA_Oc_orig$predict_travel <- probability_travel
df_PA_Oc_orig$travel_times   <- df_PA_Oc_orig$access / 60	# travel times in hours

predict_grid       <- tab_coef_Oc['intercept',] + tab_coef_Oc['grid_log',]*df_PA_Oc$grid_log
probability_grid   <- 1/(1 + exp(-predict_grid))
df_PA_Oc_orig$predict_grid <- probability_grid
df_PA_Oc_orig$grid_km      <- df_PA_Oc_orig$grid_km/1000	# m to km

# set up plots
empty     <- data.frame()	# short natural was not included in model for SAm
Oc_rsds   <- ggplot(empty) + geom_point() + theme_minimal() + theme(plot.title=element_text(size=18),axis.title.y = element_text(size = 16)) + ylab('Oceania') 

Oc_travel <- ggplot(df_PA_Oc_orig,aes(x=travel_times,y=predict_travel)) + geom_point() + theme_settings+ ylab('') + xlab('') + scale_y_continuous(limits=c(0,0.3)) + xlab('') + scale_x_continuous(limits = c(0,80))

Oc_grid   <- ggplot(df_PA_Oc_orig,aes(x=grid_km,y=predict_grid)) + geom_point() + theme_settings+ ylab('') + xlab('') + scale_y_continuous(limits=c(0,0.3)) + scale_x_continuous(limits=c(0,1000)) + xlab('') # excludes 13 

empty     <- data.frame()	# short natural was not included in model for SAm
Oc_short  <- ggplot(empty) + geom_point() + theme_minimal() + theme(plot.title=element_text(size=18))


### create full plot--------------------------------------------------------------------------------------
fullplot <- ggarrange(NAm_rsds,NAm_travel,NAm_grid,NAm_short,SAm_rsds,SAm_travel,SAm_grid,SAm_short,Eur_rsds,Eur_travel,Eur_grid,Eur_short,Afr_rsds,Afr_travel,Afr_grid,Afr_short,Oc_rsds,Oc_travel,Oc_grid,Oc_short,Asia_rsds,Asia_travel,Asia_grid,Asia_short,ncol=4,nrow=6)
fullplot

pdf(file='Plot_partial_dependence.pdf',width=10,height=12)
fullplot
dev.off()
