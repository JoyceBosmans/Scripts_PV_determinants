pacman::p_load(ggplot2, spData, sp, sf, raster,ggpubr,inlmisc)

### load our probability maps (see e.g. SolarWindPotentials_Asia/Script_create_probability_map.R)
### ------------------------------------------------------------------------------------------------------
raster_Asia_full  <- raster('/vol/milkunB/jbosmans/PV_Konrad_Asia/Probability_map_Asia.gri')
raster_Eur_full   <- raster('/vol/milkunB/jbosmans/PV_Konrad_Eur/Probability_map_Eur.gri')
raster_NAm_full   <- raster('/vol/milkunB/jbosmans/PV_Konrad_NAm/Probability_map_NAm.gri')
raster_SAm_full   <- raster('/vol/milkunB/jbosmans/PV_Konrad_SAm/Probability_map_SAm.gri')
raster_Afr_full   <- raster('/vol/milkunB/jbosmans/PV_Konrad_Afr/Probability_map_Afr_2.gri')
raster_Oc_full    <- raster('/vol/milkunB/jbosmans/PV_Konrad_Oc/Probability_map_Oc.gri')

# load polygons to use on IMAGE maps
polygon_region_NAm  <- st_read('/vol/milkunB/jbosmans/SolarWindPotentials_NAm/polygon_NAm.shp')
polygon_region_SAm  <- st_read('/vol/milkunB/jbosmans/SolarWindPotentials_SAm/polygon_SAm.shp')
polygon_region_Eur  <- st_read('/vol/milkunB/jbosmans/SolarWindPotentials_Eur/polygon_europe.shp')
polygon_region_Asia <- st_read('/vol/milkunB/jbosmans/SolarWindPotentials_Asia/polygon_Asia.shp')
polygon_region_Afr  <- st_read('/vol/milkunB/jbosmans/SolarWindPotentials_Afr/polygon_Afr.shp')
polygon_region_Oc   <- st_read('/vol/milkunB/jbosmans/PV_determinants_Oc/polygon_Oceania.shp')


### load IMAGE maps. See IMAGE_suitability/Notes_David*
### -------------------------------------------------------------------------------------------------------
global_PV_COE_LF  <- raster('/vol/milkunB/jbosmans/IMAGE_suitability/PV_COE_LF.asc')	# total cost / tech_pot
global_PV_COE_PLh <- raster('/vol/milkunB/jbosmans/IMAGE_suitability/PV_COE_PLh.asc')   # LF plus transmission cost

# To do: set PoO rasters to resolution of COE. find COE over land only, filling NA with max cost + 1

### NAm --------------------------------------
NAm_PV_COE_PLh  <- mask(global_PV_COE_PLh,polygon_region_NAm)
NAm_PV_COE_PLh  <- crop(NAm_PV_COE_PLh,extent(-179.1444, -50, 14.55556, 83.11111))		# same extent as raster_NAm_full
raster_NAm_full <- crop(raster_NAm_full,extent(-179.1444, -50, 14.55556, 83.11111))
plot(NAm_PV_COE_PLh) 

raster_NAm_coarse <- resample(raster_NAm_full,NAm_PV_COE_PLh,method='bilinear') 

# then fill NA values in COE where raster_coarse is not NA and keep grid cells where both have values.
maxcost               <- raster_NAm_coarse
maxcost[maxcost >= 0] <- maxValue(NAm_PV_COE_PLh) + 1
NAm_PV_COE_PLh        <- cover(NAm_PV_COE_PLh,maxcost)

df_NAm_coarse         <- as.data.frame(raster_NAm_coarse,xy=TRUE,na.rm=TRUE)
df_NAm_PV_COE         <- as.data.frame(NAm_PV_COE_PLh,xy=TRUE,na.rm=TRUE)

nrow(df_NAm_coarse)
nrow(df_NAm_PV_COE)
colnames(df_NAm_coarse)[colnames(df_NAm_coarse) == 'layer'] <- 'probability'
colnames(df_NAm_PV_COE)[colnames(df_NAm_PV_COE) == ''] <- 'COE'
df_NAm_coarse$COE <- df_NAm_PV_COE$COE

theme_settings <-  list(theme_bw(),theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank()), theme(panel.border= element_blank()), theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5)),theme(axis.text=element_text(size=14),axis.title=element_text(size=16),plot.title = element_text(size=18)), ylim(0,0.35),scale_x_continuous(limits = c(1, 0), trans="reverse") )

spearman_cor      <- round(cor(-df_NAm_coarse$COE,df_NAm_coarse$probability,method='spearman'), digits=2)
plot_scatter_NAm  <- ggplot(df_NAm_coarse, aes(x=probability, y=COE)) + theme_bw()  + geom_point() + theme_settings + labs(y= "COE ($/kWh)", x = "Probability") + ggtitle('North America') + geom_text(x=-0.3, y=0.02, label=paste('rho','==',spearman_cor), parse=TRUE,size=7)


### SAm ----------------------------------------
SAm_PV_COE_PLh  <- mask(global_PV_COE_PLh,polygon_region_SAm)
SAm_PV_COE_PLh  <- crop(SAm_PV_COE_PLh,extent(-109.4444, -32.4, -55.9, 32.37778))	# same extent as raster_SAm_full
plot(SAm_PV_COE_PLh) 

raster_SAm_coarse <- resample(raster_SAm_full,SAm_PV_COE_PLh,method='bilinear') 

# then fill NA values in COE where raster_coarse is not NA and keep grid cells where both have values.
maxcost               <- raster_SAm_coarse
maxcost[maxcost >= 0] <- maxValue(SAm_PV_COE_PLh) + 1
SAm_PV_COE_PLh        <- cover(SAm_PV_COE_PLh,maxcost)

df_SAm_coarse         <- as.data.frame(raster_SAm_coarse,xy=TRUE,na.rm=TRUE)
df_SAm_PV_COE         <- as.data.frame(SAm_PV_COE_PLh,xy=TRUE,na.rm=TRUE)

colnames(df_SAm_coarse)[colnames(df_SAm_coarse) == 'prediction'] <- 'probability'
colnames(df_SAm_PV_COE)[colnames(df_SAm_PV_COE) == ''] <- 'COE'

df_SAm_coarse$COE <- df_SAm_PV_COE$COE

spearman_cor      <- round(cor(-df_SAm_coarse$COE,df_SAm_coarse$probability,method='spearman'), digits=2)
plot_scatter_SAm  <- ggplot(df_SAm_coarse, aes(x=probability, y=COE)) + theme_bw()  + geom_point() + theme_settings + labs(y= "COE ($/kWh)", x = "Probability") + ggtitle('South America') + geom_text(x=-0.3, y=0.02, label=paste('rho','==',spearman_cor), parse=TRUE,size=7)


### EUrope -----------------------------------
Eur_PV_COE_PLh  <- mask(global_PV_COE_PLh,polygon_region_Eur)
Eur_PV_COE_PLh  <- crop(Eur_PV_COE_PLh,extent(-73, 44.82222, 27.6444, 83.57778))	# Eure extent as raster_Eur_full
plot(Eur_PV_COE_PLh) 

raster_Eur_coarse <- resample(raster_Eur_full,Eur_PV_COE_PLh,method='bilinear') 

# then fill NA values in COE where raster_coarse is not NA and keep grid cells where both have values.
maxcost               <- raster_Eur_coarse
maxcost[maxcost >= 0] <- maxValue(Eur_PV_COE_PLh) + 1
Eur_PV_COE_PLh        <- cover(Eur_PV_COE_PLh,maxcost)

df_Eur_coarse         <- as.data.frame(raster_Eur_coarse,xy=TRUE,na.rm=TRUE)
df_Eur_PV_COE         <- as.data.frame(Eur_PV_COE_PLh,xy=TRUE,na.rm=TRUE)

colnames(df_Eur_coarse)[colnames(df_Eur_coarse) == 'prediction'] <- 'probability'
colnames(df_Eur_PV_COE)[colnames(df_Eur_PV_COE) == ''] <- 'COE'

df_Eur_coarse$COE <- df_Eur_PV_COE$COE

spearman_cor      <- round(cor(-df_Eur_coarse$COE,df_Eur_coarse$probability,method='spearman'), digits=2)
plot_scatter_Eur  <- ggplot(df_Eur_coarse, aes(x=probability, y=COE)) + theme_bw()  + geom_point() + theme_settings + labs(y= "COE ($/kWh)", x = "Probability") + ggtitle('Europe') + geom_text(x=-0.3, y=0.02, label=paste('rho','==',spearman_cor), parse=TRUE,size=7)

### ASIA -----------------------------------
extent(raster_Asia_full) <- extent(-180, 180, -11.63333, 81.85556)
x1              <- crop(raster_Asia_full, extent(-180, 0, -11.63333, 81.85556))
x2              <- crop(raster_Asia_full, extent(0, 180, -11.63333, 81.85556))
extent(x1)      <- c(180, 360, -11.63333, 81.85556)
raster_Asia_full_360  <- merge(x1, x2)

raster_Asia_full_360  <- crop(raster_Asia_full_360,extent(20,200,-11.63333, 81.85556))

Asia_PV_COE_PLh <- mask(global_PV_COE_PLh,polygon_region_Asia)			# select Asia and shift longitudes as above
x1              <- crop(Asia_PV_COE_PLh, extent(-180, 0, -90, 90))
x2              <- crop(Asia_PV_COE_PLh, extent(0, 180, -90, 90))   
extent(x1)      <- c(180, 360, -90, 90)
Asia_PV_COE_PLh <- merge(x1, x2)
Asia_PV_COE_PLh <- crop(Asia_PV_COE_PLh,extent(20,200,-12,82))

raster_Asia_coarse <- resample(raster_Asia_full_360,Asia_PV_COE_PLh,method='bilinear') 

# then fill NA values in COE where raster_coarse is not NA and keep grid cells where both have values.
maxcost               <- raster_Asia_coarse					# 27019 cells while PV_COE has 27119
maxcost[maxcost >= 0] <- maxValue(Asia_PV_COE_PLh) + 1
Asia_PV_COE_PLh       <- cover(Asia_PV_COE_PLh,maxcost)

# some mismatches occur on the coasts. Remove cells where values are missing from either map (mostly PoO map)
Asia_PV_COE_PLh[is.na(raster_Asia_coarse)] <- NA

df_Asia_coarse        <- as.data.frame(raster_Asia_coarse,xy=TRUE,na.rm=TRUE)
df_Asia_PV_COE        <- as.data.frame(Asia_PV_COE_PLh,xy=TRUE,na.rm=TRUE)

#~ colnames(df_Asia_coarse)[colnames(df_Asia_coarse) == 'r_tmp_2021-12-16_151110_18666_19168'] <- 'probability'
colnames(df_Asia_PV_COE)[colnames(df_Asia_PV_COE) == ''] <- 'COE'

df_Asia_coarse$COE <- df_Asia_PV_COE$COE
colnames(df_Asia_coarse) <- c('x','y','probability','COE')

spearman_cor      <- round(cor(-df_Asia_coarse$COE,df_Asia_coarse$probability,method='spearman'), digits=2)
plot_scatter_Asia  <- ggplot(df_Asia_coarse, aes(x=probability, y=COE)) + theme_bw()  + geom_point() + theme_settings + labs(y= "COE ($/kWh)", x = "Probability") + ggtitle('Asia') + geom_text(x=-0.3, y=0.02, label=paste('rho','==',spearman_cor), parse=TRUE,size=7)

### Oceania------------------------------------------------
extent(raster_Oc_full) <- extent(-180, 180, -54.75556, 18.8)
x1              <- crop(raster_Oc_full, extent(-180, 0, -54.75556, 18.8))
x2              <- crop(raster_Oc_full, extent(0, 180, -54.75556, 18.8))
extent(x1)      <- c(180, 360, -54.75556, 18.8)
raster_Oc_full_360  <- merge(x1, x2)

raster_Oc_full_360  <- crop(raster_Oc_full_360,extent(10,200, -54.75556, 18.8))

Oc_PV_COE_PLh  <- mask(global_PV_COE_PLh,polygon_region_Oc)			# select Oc and shift longitudes as above
x1             <- crop(Oc_PV_COE_PLh, extent(-180, 0, -90, 90))
x2             <- crop(Oc_PV_COE_PLh, extent(0, 180, -90, 90))   
extent(x1)     <- c(180, 360, -90, 90)
Oc_PV_COE_PLh  <- merge(x1, x2)
Oc_PV_COE_PLh  <- crop(Oc_PV_COE_PLh,extent(10,200, -55, 19))

raster_Oc_coarse <- resample(raster_Oc_full_360,Oc_PV_COE_PLh,method='bilinear') 

#~ Oc_PV_COE_PLh <- mask(global_PV_COE_PLh,polygon_region_Oc)	
#~ Oc_PV_COE_PLh <- crop(Oc_PV_COE_PLh,extent( -179.8778, 179.8556, -54.75556, 18.8))	# Oc extent as raster_Oc_full
plot(Oc_PV_COE_PLh) 

#~ raster_Oc_coarse <- resample(raster_Oc_full,Oc_PV_COE_PLh,method='bilinear') 

# then fill NA values in COE where raster_coarse is not NA and keep grid cells where both have values.
maxcost               <- raster_Oc_coarse
maxcost[maxcost >= 0] <- maxValue(Oc_PV_COE_PLh) + 1
Oc_PV_COE_PLh        <- cover(Oc_PV_COE_PLh,maxcost)

df_Oc_coarse         <- as.data.frame(raster_Oc_coarse,xy=TRUE,na.rm=TRUE)
df_Oc_PV_COE         <- as.data.frame(Oc_PV_COE_PLh,xy=TRUE,na.rm=TRUE)

colnames(df_Oc_coarse)[colnames(df_Oc_coarse) == 'r_tmp_2022-09-28_151720_4831_50127'] <- 'probability'
colnames(df_Oc_PV_COE)[colnames(df_Oc_PV_COE) == ''] <- 'COE'

df_Oc_coarse$COE <- df_Oc_PV_COE$COE

spearman_cor     <- round(cor(-df_Oc_coarse$COE,df_Oc_coarse$probability,method='spearman'), digits=2)
plot_scatter_Oc  <- ggplot(df_Oc_coarse, aes(x=probability, y=COE)) + theme_bw()  + geom_point() + theme_settings + labs(y= "COE ($/kWh)", x = "Probability") + ggtitle('Oceania') + geom_text(x=-0.3, y=0.02, label=paste('rho','==',spearman_cor), parse=TRUE,size=7)

### Africa -------------------------------------------------
print('NOTE check extent and colnames')
Afr_PV_COE_PLh  <- mask(global_PV_COE_PLh,polygon_region_Afr)
Afr_PV_COE_PLh  <- crop(Afr_PV_COE_PLh,extent(-26, 64, -41, 38))	# Afre extent as raster_Afr_full
plot(Afr_PV_COE_PLh) 

raster_Afr_coarse <- resample(raster_Afr_full,Afr_PV_COE_PLh,method='bilinear') 

# then fill NA values in COE where raster_coarse is not NA and keep grid cells where both have values.
maxcost               <- raster_Afr_coarse
maxcost[maxcost >= 0] <- maxValue(Afr_PV_COE_PLh) + 1
Afr_PV_COE_PLh        <- cover(Afr_PV_COE_PLh,maxcost)

df_Afr_coarse         <- as.data.frame(raster_Afr_coarse,xy=TRUE,na.rm=TRUE)
df_Afr_PV_COE         <- as.data.frame(Afr_PV_COE_PLh,xy=TRUE,na.rm=TRUE)

colnames(df_Afr_coarse)[colnames(df_Afr_coarse) == 'prediction'] <- 'probability'
colnames(df_Afr_PV_COE)[colnames(df_Afr_PV_COE) == ''] <- 'COE'

df_Afr_coarse$COE <- df_Afr_PV_COE$COE

spearman_cor      <- round(cor(-df_Afr_coarse$COE,df_Afr_coarse$probability,method='spearman'), digits=2)
plot_scatter_Afr  <- ggplot(df_Afr_coarse, aes(x=probability, y=COE)) + theme_bw()  + geom_point() + theme_settings + labs(y= "COE ($/kWh)", x = "Probability") + ggtitle('Africa') + geom_text(x=-0.3, y=0.02, label=paste('rho','==',spearman_cor), parse=TRUE,size=7)


### full plot-----------------------------------------------
fullplot <- ggarrange(plot_scatter_NAm,plot_scatter_Eur,plot_scatter_SAm,plot_scatter_Afr,plot_scatter_Asia,plot_scatter_Oc,ncol=2,nrow=3)

pdf('Plot_compare_PoO_COE.pdf',width=10,height=14)
fullplot
dev.off()
system('gv Plot_compare_PoO_COE.pdf')
