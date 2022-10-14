pacman::p_load(ggplot2, spData, sp, sf, raster,ggpubr,inlmisc,gridExtra)

# load polygons to use on IMAGE maps
polygon_region_NAm  <- st_read('/vol/milkunB/jbosmans/SolarWindPotentials_NAm/polygon_NAm.shp')
polygon_region_SAm  <- st_read('/vol/milkunB/jbosmans/SolarWindPotentials_SAm/polygon_SAm.shp')
polygon_region_Eur  <- st_read('/vol/milkunB/jbosmans/SolarWindPotentials_Eur/polygon_europe.shp')
polygon_region_Asia <- st_read('/vol/milkunB/jbosmans/PV_determinants_Asia/polygon_Asia.shp')
polygon_region_Afr  <- st_read('/vol/milkunB/jbosmans/SolarWindPotentials_Afr/polygon_Afr.shp')
polygon_region_Oc   <- st_read('/vol/milkunB/jbosmans/PV_determinants_Oc/polygon_Oceania.shp')

### load IMAGE maps. See IMAGE_suitability/Notes_David*
### -------------------------------------------------------------------------------------------------------
global_PV_COE_LF  <- raster('/vol/milkunB/jbosmans/IMAGE_suitability/PV_COE_LF.asc')	# total cost / tech_pot
global_PV_COE_PLh <- raster('/vol/milkunB/jbosmans/IMAGE_suitability/PV_COE_PLh.asc')   # LF plus transmission cost


### settings for plots
### ------------------------------------------------------------------------------------------------------
Tbreaks = 0.1
Tlim_up = 1.0
Tlim_lo = 0
color_scheme <- inlmisc::GetColors(length(seq(Tlim_lo,Tlim_up,Tbreaks)),scheme='sunset')
br <- seq((Tlim_lo),(Tlim_up),Tbreaks)
lab <- paste0(c(seq(Tlim_lo,Tlim_up,Tbreaks)))
lim <- c(Tlim_lo,Tlim_up)

Tbreaks = 0.05
Tlim_up = 0.35
Tlim_lo = 0.10
color_scheme_COE <- inlmisc::GetColors(length(seq(Tlim_lo,Tlim_up,Tbreaks)),scheme='PRGn',reverse=T)
br_COE  <- seq((Tlim_lo-Tbreaks),(Tlim_up+Tbreaks),Tbreaks)
lab_COE <-  paste0(c(paste0('<',Tlim_lo),seq(Tlim_lo,Tlim_up,Tbreaks),paste0('>',Tlim_up)),'')
lim_COE <- c(Tlim_lo-Tbreaks,Tlim_up+Tbreaks)

# set to min 5 and max 35 ct
global_PV_COE_PLh_orig <- global_PV_COE_PLh
global_PV_COE_PLh[global_PV_COE_PLh < 0.05] <- 0.05
global_PV_COE_PLh[global_PV_COE_PLh > 0.35] <- 0.35

### Create plot.allFit### -------------------------------------------------------------------------------
df_PV_COE   <- as.data.frame(global_PV_COE_PLh,xy=TRUE,na.rm=TRUE)
plot_PV_COE <- ggplot() + geom_sf(data=polygon_region_NAm,size = 0.2,fill='#666666') + geom_sf(data=polygon_region_SAm,size = 0.2,fill='#666666') + geom_sf(data=polygon_region_Eur,size = 0.2,fill='#666666') + geom_sf(data=polygon_region_Afr,size = 0.2,fill='#666666') + geom_sf(data=polygon_region_Oc,size = 0.2,fill='#666666') + geom_sf(data=polygon_region_Asia,size = 0.2,fill='#666666')  + geom_tile(data=df_PV_COE, aes(x=x, y=y, fill=PV_COE_PLh), alpha=0.8) +  theme(
    panel.background = element_blank(),axis.ticks = element_blank(),axis.title = element_blank(),axis.text = element_blank(),  legend.key.width = unit(0.8,'line'), legend.key.height = unit(3,'line'), legend.margin=margin(-1,-1,-1,-15),legend.text=element_text(size=14), legend.title=element_blank()) + scale_fill_gradientn(colors = color_scheme_COE,breaks = br_COE,labels = lab_COE,limits = lim_COE,na.value = 'transparent')  


pdf(file='Plot_IMAGE_COE_global.pdf')
plot_PV_COE
dev.off() 


