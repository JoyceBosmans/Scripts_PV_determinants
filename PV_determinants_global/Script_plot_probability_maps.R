pacman::p_load(ggplot2, spData, sp, sf, raster,ggpubr,inlmisc,data.table,gridExtra)

### load our probability maps (see e.g. SolarWindPotentials_Asia/Script_create_probability_map.R)
### ------------------------------------------------------------------------------------------------------
raster_Asia_full  <- raster('/vol/milkunB/jbosmans/PV_determinants_Asia/Probability_map_Asia_.gri')
raster_Asia_fixed <- raster('/vol/milkunB/jbosmans/PV_determinants_Asia/Probability_map_Asia_fixed.gri')

raster_Eur_full   <- raster('/vol/milkunB/jbosmans/PV_determinants_Eur/Probability_map_Eur_.gri')
raster_Eur_fixed  <- raster('/vol/milkunB/jbosmans/PV_determinants_Eur/Probability_map_Eur_fixed.gri')

raster_NAm_full   <- raster('/vol/milkunB/jbosmans/PV_determinants_NAm/Probability_map_NAm.gri')
raster_NAm_fixed  <- raster('/vol/milkunB/jbosmans/PV_determinants_NAm/Probability_map_NAm_fixed.gri')

raster_SAm_full  <- raster('/vol/milkunB/jbosmans/PV_determinants_SAm/Probability_map_SAm.gri')
raster_SAm_fixed <- raster('/vol/milkunB/jbosmans/PV_determinants_SAm/Probability_map_SAm_fixed.gri')

# load polygons to use on IMAGE maps
polygon_region_NAm  <- st_read('/vol/milkunB/jbosmans/SolarWindPotentials_NAm/polygon_NAm.shp')
polygon_region_SAm  <- st_read('/vol/milkunB/jbosmans/SolarWindPotentials_SAm/polygon_SAm.shp')
polygon_region_Eur  <- st_read('/vol/milkunB/jbosmans/SolarWindPotentials_Eur/polygon_europe.shp')
polygon_region_Asia <- st_read('/vol/milkunB/jbosmans/SolarWindPotentials_Asia/polygon_Asia.shp')


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

eq <- function(x,y) {
  m <- lm(y ~ x)
  as.character(
    as.expression(
      substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                list(a = format(coef(m)[1], digits = 4),
                b = format(coef(m)[2], digits = 4),
                r2 = format(summary(m)$r.squared, digits = 3)))
    )
  )
}


### Plot for Asia
### -------------------------------------------------------------------------------------------------------
# for Asia, shift longitude to 0:360 for plotting:
extent(raster_Asia_full) <- extent(-180, 180, -52.57778, 81.51111)
x1              <- crop(raster_Asia_full, extent(-180, 0, -52.57778, 81.51111))
x2              <- crop(raster_Asia_full, extent(0, 180, -52.57778, 81.51111))
extent(x1)      <- c(180, 360, -52.57778, 81.51111)
raster_Asia_full_360  <- merge(x1, x2)

#raster_Asia_full_360  <- shift(rotate(shift(raster_Asia_full, 180)), 180)
raster_Asia_full_360  <- crop(raster_Asia_full_360,extent(20,200,-50,81))

#full map 
df_Asia_full   <- as.data.frame(raster_Asia_full_360,xy=TRUE,na.rm=TRUE)
colnames(df_Asia_full) <- c("x", "y","prediction")

plot_Asia_full <- ggplot()  + geom_tile(data=df_Asia_full, aes(x=x, y=y, fill=prediction), alpha=0.8) +  theme(
    panel.background = element_blank(),axis.ticks = element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.key.width = unit(0.8,'line'), legend.key.height = unit(3,'line'), legend.margin=margin(-1,-1,-1,-15),legend.text=element_text(size=14), legend.title=element_blank()) + scale_fill_gradientn(colors = color_scheme,breaks = br,labels = lab,limits = lim,na.value = 'transparent')

### Plot for Europe
### ------------------------------------------------------------------------------------------------------
raster_Eur_full <- crop(raster_Eur_full,extent(-26,48.2222,27.6333,71.5))
df_Eur_full     <- as.data.frame(raster_Eur_full,xy=TRUE,na.rm=TRUE)
plot_Eur_full   <- ggplot() + geom_tile(data=df_Eur_full, aes(x=x, y=y, fill=prediction), alpha=0.8) +  theme(
    panel.background = element_blank(),axis.ticks = element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = "none") + scale_fill_gradientn(colors = color_scheme,breaks = br,labels = lab,limits = lim,na.value = 'transparent') 
    

### Plot for NAm
### -------------------------------------------------------------------------------------------------------
df_NAm_full     <- as.data.frame(raster_NAm_full,xy=TRUE,na.rm=TRUE)
colnames(df_NAm_full) <- c("x", "y","prediction")
plot_NAm_full   <- ggplot() + geom_tile(data=df_NAm_full, aes(x=x, y=y, fill=prediction), alpha=0.8) +  theme(
    panel.background = element_blank(),axis.ticks = element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = "none") + scale_fill_gradientn(colors = color_scheme,breaks = br,labels = lab,limits = lim,na.value = 'transparent') 
    
    
#### Plot for SAm
### ------------------------------------------------------------------------------------------------------
df_SAm_full     <- as.data.frame(raster_SAm_full,xy=TRUE,na.rm=TRUE)
plot_SAm_full   <- ggplot() + geom_tile(data=df_SAm_full, aes(x=x, y=y, fill=prediction), alpha=0.8) +  theme(
    panel.background = element_blank(),axis.ticks = element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = "none") + scale_fill_gradientn(colors = color_scheme,breaks = br,labels = lab,limits = lim,na.value = 'transparent') 


### put plots together 
all_plot <- grid.arrange(arrangeGrob(plot_NAm_full + theme(legend.position="none"), plot_Eur_full + theme(legend.position="none"),ncol=2,widths=c(1,1)), arrangeGrob(plot_SAm_full + theme(legend.position="none"), plot_Asia_full,ncol=2,widths=c(1,2.6)), nrow = 2) #,common.legend=TRUE,legend='right')

pdf(file='Plot_probability_maps.pdf')
print(as_ggplot(all_plot))
dev.off()

pdf(file='Plot_probability_SAm.pdf')
plot_SAm_full
dev.off()

pdf(file='Plot_probability_NAm.pdf')
plot_NAm_full
dev.off()

pdf(file='Plot_probability_Eur.pdf')
plot_Eur_full
dev.off()

pdf(file='Plot_probability_Asia.pdf')
plot_Asia_full
dev.off()
