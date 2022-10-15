pacman::p_load(ggplot2, spData, sp, sf, raster,ggpubr,inlmisc,gridExtra)

# load polygons to use on IMAGE maps
polygon_region_NAm  <- st_read('/vol/milkunB/jbosmans/SolarWindPotentials_NAm/polygon_NAm.shp')
polygon_region_SAm  <- st_read('/vol/milkunB/jbosmans/SolarWindPotentials_SAm/polygon_SAm.shp')
polygon_region_Eur  <- st_read('/vol/milkunB/jbosmans/SolarWindPotentials_Eur/polygon_europe.shp')
polygon_region_Asia <- st_read('/vol/milkunB/jbosmans/PV_determinants_Asia/polygon_Asia.shp')

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


### Plots for North America -----------------------------------------------------------------------------
    
# IMAGE COE incl. transmission
NAm_PV_COE_PLh  <- mask(global_PV_COE_PLh,polygon_region_NAm)
NAm_PV_COE_PLh  <- crop(NAm_PV_COE_PLh,extent(-179.1444, -50, 14.55556, 80.46667))
df_NAm_PV_COE   <- as.data.frame(NAm_PV_COE_PLh,xy=TRUE,na.rm=TRUE)
plot_NAm_PV_COE <- ggplot() + geom_sf(data=polygon_region_NAm,size = 0.2,fill='#666666') + coord_sf(xlim = c(-179.1444, -50), ylim = c(14.55556, 80.46667), expand = FALSE) + geom_tile(data=df_NAm_PV_COE, aes(x=x, y=y, fill=PV_COE_PLh), alpha=0.8) +  theme(
    panel.background = element_blank(),axis.ticks = element_blank(),axis.title = element_blank(),axis.text = element_blank(),  legend.key.width = unit(0.8,'line'), legend.key.height = unit(3,'line'), legend.margin=margin(-1,-1,-1,-15),legend.text=element_text(size=14), legend.title=element_blank()) + scale_fill_gradientn(colors = color_scheme_COE,breaks = br_COE,labels = lab_COE,limits = lim_COE,na.value = 'transparent') 

### Plots for South America -----------------------------------------------------------------------------

# IMAGE COE incl. transmission
SAm_PV_COE_PLh  <- mask(global_PV_COE_PLh,polygon_region_SAm)
SAm_PV_COE_PLh  <- crop(SAm_PV_COE_PLh,extent(-92.24444, -32.38889, -55.91111, 32.37778))
df_SAm_PV_COE   <- as.data.frame(SAm_PV_COE_PLh,xy=TRUE,na.rm=TRUE)
plot_SAm_PV_COE <- ggplot() + geom_sf(data=polygon_region_SAm,size = 0.2,fill='#666666') + coord_sf(xlim = c(-92.24444, -32.38889), ylim = c(-55.91111, 32.37778), expand = FALSE) + geom_tile(data=df_SAm_PV_COE, aes(x=x, y=y, fill=PV_COE_PLh), alpha=0.8) +  theme(
    panel.background = element_blank(),axis.ticks = element_blank(),axis.title = element_blank(),axis.text = element_blank(),  legend.key.width = unit(0.8,'line'), legend.key.height = unit(3,'line'), legend.margin=margin(-1,-1,-1,-15),legend.text=element_text(size=14), legend.title=element_blank()) + scale_fill_gradientn(colors = color_scheme_COE,breaks = br_COE,labels = lab_COE,limits = lim_COE,na.value = 'transparent') 
 
### Plots for Europe
### ------------------------------------------------------------------------------------------------------

# IMAGE COE incl. transmission
Eur_PV_COE_PLh  <- mask(global_PV_COE_PLh,polygon_region_Eur)
Eur_PV_COE_PLh  <- crop(Eur_PV_COE_PLh,extent(-35,48.2222,27.6333,71.5))
df_Eur_PV_COE   <- as.data.frame(Eur_PV_COE_PLh,xy=TRUE,na.rm=TRUE)
plot_Eur_PV_COE <- ggplot() + geom_sf(data=polygon_region_Eur,size = 0.2,fill='#666666') + coord_sf(xlim = c(-35, 48.2222), ylim = c(27.6333, 71.5), expand = FALSE) + geom_tile(data=df_Eur_PV_COE, aes(x=x, y=y, fill=PV_COE_PLh), alpha=0.8) +  theme(
    panel.background = element_blank(),axis.ticks = element_blank(),axis.title = element_blank(),axis.text = element_blank(),  legend.key.width = unit(0.8,'line'), legend.key.height = unit(3,'line'), legend.margin=margin(-1,-1,-1,-15),legend.text=element_text(size=14), legend.title=element_blank()) + scale_fill_gradientn(colors = color_scheme_COE,breaks = br_COE,labels = lab_COE,limits = lim_COE,na.value = 'transparent') 
    
### Plots for Asia
### ------------------------------------------------------------------------------------------------------

Asia_PV_COE_PLh <- mask(global_PV_COE_PLh,polygon_region_Asia)			# select Asia and shift longitudes as above
x1              <- crop(Asia_PV_COE_PLh, extent(-180, 0, -90, 90))
x2              <- crop(Asia_PV_COE_PLh, extent(0, 180, -90, 90))   
extent(x1)      <- c(180, 360, -90, 90)
Asia_PV_COE_PLh <- merge(x1, x2)
Asia_PV_COE_PLh <- crop(Asia_PV_COE_PLh,extent(20,200,-50,81))

# IMAGE COE incl. transmission
df_Asia_PV_COE   <- as.data.frame(Asia_PV_COE_PLh,xy=TRUE,na.rm=TRUE)
colnames(df_Asia_PV_COE) <- c('x','y','PV_COE_PLh')
plot_Asia_PV_COE <- ggplot() + geom_sf(data=polygon_region_Asia,size = 0.2,fill='#666666') + coord_sf(xlim = c(20, 200), ylim = c(-50, 81), expand = FALSE) + geom_tile(data=df_Asia_PV_COE, aes(x=x, y=y, fill=PV_COE_PLh), alpha=0.8) +  theme(
    panel.background = element_blank(),axis.ticks = element_blank(),axis.title = element_blank(),axis.text = element_blank(),  legend.key.width = unit(0.8,'line'), legend.key.height = unit(3,'line'), legend.margin=margin(-1,-1,-1,-15),legend.text=element_text(size=14), legend.title=element_blank()) + scale_fill_gradientn(colors = color_scheme_COE,breaks = br_COE,labels = lab_COE,limits = lim_COE,na.value = 'transparent') 

# HERE: put together
all_plot <- grid.arrange(arrangeGrob(plot_NAm_PV_COE + theme(legend.position="none"), plot_Eur_PV_COE + theme(legend.position="none"),ncol=2,widths=c(1,1)), arrangeGrob(plot_SAm_PV_COE+ theme(legend.position="none"), plot_Asia_PV_COE,ncol=2,widths=c(1,2.7)), nrow = 2) #,common.legend=TRUE,legend='right')

#grid_arrange_shared_legend(arrangeGrob(plot_NAm_PV_COE, plot_Eur_PV_COE,ncol=2,widths=c(1.5,1)), arrangeGrob(plot_SAm_PV_COE, plot_Asia_PV_COE,ncol=2,widths=c(1,2)), nrow = 2,position=c('right'))

pdf(file='Plot_IMAGE_COE.pdf')
grid.arrange(arrangeGrob(plot_NAm_PV_COE + theme(legend.position="none"), plot_Eur_PV_COE + theme(legend.position="none"),ncol=2,widths=c(1,1)), arrangeGrob(plot_SAm_PV_COE+ theme(legend.position="none"), plot_Asia_PV_COE,ncol=2,widths=c(1,2.7)), nrow = 2)
dev.off() 
