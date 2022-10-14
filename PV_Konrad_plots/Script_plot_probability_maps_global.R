pacman::p_load(ggplot2, spData, sp, sf, raster,ggpubr,inlmisc,data.table,gridExtra,tcltk)



### load our probability maps 
### ------------------------------------------------------------------------------------------------------
raster_Asia_full  <- raster('/vol/milkunB/jbosmans/PV_Konrad_Asia/Probability_map_Asia.gri')
raster_Eur_full   <- raster('/vol/milkunB/jbosmans/PV_Konrad_Eur/Probability_map_Eur.gri')
raster_NAm_full   <- raster('/vol/milkunB/jbosmans/PV_Konrad_NAm/Probability_map_NAm.gri')
raster_SAm_full   <- raster('/vol/milkunB/jbosmans/PV_Konrad_SAm/Probability_map_SAm.gri')
raster_Oc_full    <- raster('/vol/milkunB/jbosmans/PV_Konrad_Oc/Probability_map_Oc.gri')
raster_Afr_full   <- raster('/vol/milkunB/jbosmans/PV_Konrad_Afr/Probability_map_Afr_2.gri')

### settings for plots
### ------------------------------------------------------------------------------------------------------
Tbreaks = 0.1
Tlim_up = 1.0
Tlim_lo = 0
color_scheme <- inlmisc::GetColors(length(seq(Tlim_lo,Tlim_up,Tbreaks)),scheme='sunset')
color_scheme_rank <- inlmisc::GetColors(length(seq(Tlim_lo,Tlim_up,Tbreaks)),scheme='smooth rainbow')
br <- seq((Tlim_lo),(Tlim_up),Tbreaks)
lab <- paste0(c(seq(Tlim_lo,Tlim_up,Tbreaks)))
lim <- c(Tlim_lo,Tlim_up)

#
### Plot for Africa
### ------------------------------------------------------------------------------------------------------

df_Afr     <- as.data.frame(raster_Afr_full,xy=TRUE,na.rm=TRUE)

### rank
#df_Afr_full      <- as.data.frame(raster_Afr_full,xy=TRUE,na.rm=TRUE)	# determine rank on full map
df_Afr$rank <- NA
df_Afr$rank[order(df_Afr$prediction)]  <- 1:nrow(df_Afr)
df_Afr$rank <- df_Afr$rank / nrow(df_Afr)	#0-1 scale
print('df Africa')
quantile(df_Afr$y,probs=c(0,0.5,1))
df_Afr[0:5,]

### Plot for Europe
### ------------------------------------------------------------------------------------------------------
raster_Eur_noGr <- crop(raster_Eur_full,extent(-26,48.2222,27.6333,71.5))
df_Eur_noGr     <- as.data.frame(raster_Eur_full,xy=TRUE,na.rm=TRUE)
df_Eur_noGr     <- df_Eur_noGr[df_Eur_noGr$y<68 | df_Eur_noGr$x>-20,]	# for plotting leave out Greenland
#~ plot_Eur_full   <- ggplot() + geom_tile(data=df_Eur_noGr, aes(x=x, y=y, fill=prediction), alpha=0.8) +  theme(
#~     panel.background = element_blank(),axis.ticks = element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.key.width = unit(0.8,'line'), legend.key.height = unit(3,'line'), legend.margin=margin(-1,-1,-1,-15),legend.text=element_text(size=14), legend.title=element_blank()) + scale_fill_gradientn(colors = color_scheme,breaks = br,labels = lab,limits = lim,na.value = 'transparent') 
     
### rank
df_Eur_full      <- as.data.frame(raster_Eur_full,xy=TRUE,na.rm=TRUE)	# determine rank on full map
df_Eur_full$rank <- NA
df_Eur_full$rank[order(df_Eur_full$prediction)]  <- 1:nrow(df_Eur_full)
df_Eur_full$rank <- df_Eur_full$rank / nrow(df_Eur_full)	#0-1 scale
df_Eur_noGr      <- df_Eur_full[df_Eur_full$y<68 | df_Eur_full$x>-20,]	# for plotting leave out Greenland
df_Eur_noGr      <- df_Eur_noGr[df_Eur_noGr$y<71 | df_Eur_noGr$x>-9,]	# for plotting leave out Greenland
df_Eur_noGr      <- df_Eur_noGr[df_Eur_noGr$y<66 | df_Eur_noGr$x>-24,]	# for plotting leave out Greenland
df_Eur_noGr      <- df_Eur_noGr[df_Eur_noGr$y<59 | df_Eur_noGr$x>-30,]	# for plotting leave out Greenland


#~ plot_Eur_rank   <- ggplot() + geom_tile(data=df_Eur_noGr, aes(x=x, y=y, fill=rank), alpha=0.8) +  theme(
#~     panel.background = element_blank(),axis.ticks = element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.key.width = unit(0.8,'line'), legend.key.height = unit(3,'line'), legend.margin=margin(-1,-1,-1,-15),legend.text=element_text(size=14), legend.title=element_blank()) + scale_fill_gradientn(colors = color_scheme_rank,breaks = br,labels = lab,limits = lim,na.value = 'transparent') + coord_sf(xlim = c(-26, 48), ylim = c(27, 72))
    
print('df Europe')
quantile(df_Eur_noGr$y,probs=c(0,0.5,1))
df_Eur_noGr[0:5,]


### Plot for Oceania
### -----------------------------------------------------------------------------------------------------
# for Oceania, shift longitude to 0:360 for plotting:
extent(raster_Oc_full) <- extent(-180, 180, -54.75556, 18.8)
x1              <- crop(raster_Oc_full, extent(-180, 0, -54.75556, 18.8))
x2              <- crop(raster_Oc_full, extent(0, 180, -54.75556, 18.8))
extent(x1)      <- c(180, 360, -54.75556, 18.8)
raster_Oc_full_360  <- merge(x1, x2)

raster_Oc_full_360  <- crop(raster_Oc_full_360,extent(100,200,-54.75556, 18.8))

df_Oc_full   <- as.data.frame(raster_Oc_full_360,xy=TRUE,na.rm=TRUE)
colnames(df_Oc_full) <- c("x", "y","prediction")


df_Oc_full$rank <- NA
df_Oc_full$rank[order(df_Oc_full$prediction)]  <- 1:nrow(df_Oc_full)
df_Oc_full$rank <- df_Oc_full$rank / nrow(df_Oc_full)	#0-1 scale

print('df Oceania')
quantile(df_Oc_full$y,probs=c(0,0.5,1))
df_Oc_full[0:5,]


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

#~ df_Asia_full$x_new <- df_Asia_full$x
#~ df_Asia_full$x_new[df_Asia_full$x_new<0] <- df_Asia_full$x_new[df_Asia_full$x_new<0] + 360


### rank
df_Asia_full$rank <- NA
df_Asia_full$rank[order(df_Asia_full$prediction)]  <- 1:nrow(df_Asia_full)
df_Asia_full$rank <- df_Asia_full$rank / nrow(df_Asia_full)	#0-1 scale

print('df Asia')
quantile(df_Asia_full$y,probs=c(0,0.5,1))
df_Asia_full[0:5,]



### Plot for NAm
### -------------------------------------------------------------------------------------------------------
raster_NAm_full <- crop(raster_NAm_full,extent(-179.1444, -40, 14.55556, 83.11111))
df_NAm_full     <- as.data.frame(raster_NAm_full,xy=TRUE,na.rm=TRUE)
colnames(df_NAm_full) <- c("x", "y","prediction")

### rank
df_NAm_full$rank <- NA
df_NAm_full$rank[order(df_NAm_full$prediction)]  <- 1:nrow(df_NAm_full)
df_NAm_full$rank <- df_NAm_full$rank / nrow(df_NAm_full)	#0-1 scale

print('df N Am')
quantile(df_NAm_full$y,probs=c(0,0.5,1))
df_NAm_full[0:5,]

df_rank_all <- rbind(df_Asia_full,df_Eur_noGr)

df_rank_all[0:5,]

plot_all_rank   <- ggplot() + geom_tile(data=df_rank_all, aes(x=x, y=y, fill=rank), alpha=0.8) +  theme(
    panel.background = element_blank(),axis.ticks = element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.key.width = unit(0.8,'line'), legend.key.height = unit(3,'line'), legend.margin=margin(-1,-1,-1,-15),legend.text=element_text(size=14), legend.title=element_blank()) + scale_fill_gradientn(colors = color_scheme_rank,breaks = br,labels = lab,limits = lim,na.value = 'transparent') 
    
#~ x11()
#~ plot_all_rank
#~ capture <- tk_messageBox(message = 'close plot?', detail = 'blabla')

pdf(file='Plot_rank_global.pdf')
plot_all_rank
dev.off()

print('STOP FOR NOW')
quit(save='no')

#### Plot for SAm
### ------------------------------------------------------------------------------------------------------
df_SAm_full     <- as.data.frame(raster_SAm_full,xy=TRUE,na.rm=TRUE)
plot_SAm_full   <- ggplot() + geom_tile(data=df_SAm_full, aes(x=x, y=y, fill=prediction), alpha=0.8) +  theme(
    panel.background = element_blank(),axis.ticks = element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = "none") + scale_fill_gradientn(colors = color_scheme,breaks = br,labels = lab,limits = lim,na.value = 'transparent') 

### rank
df_SAm_full$rank <- NA
df_SAm_full$rank[order(df_SAm_full$prediction)]  <- 1:nrow(df_SAm_full)
df_SAm_full$rank <- df_SAm_full$rank / nrow(df_SAm_full)	#0-1 scale

print('df S Am')
df_SAm_full[0:5,]


### global plot
### --------------------------------------------------------------------
df_rank_all <- rbind(df_SAm_full,df_NAm_full)

plot_all_rank   <- ggplot() + geom_tile(data=test, aes(x=x, y=y, fill=rank), alpha=0.8) +  theme(
    panel.background = element_blank(),axis.ticks = element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.key.width = unit(0.8,'line'), legend.key.height = unit(3,'line'), legend.margin=margin(-1,-1,-1,-15),legend.text=element_text(size=14), legend.title=element_blank()) + scale_fill_gradientn(colors = color_scheme_rank,breaks = br,labels = lab,limits = lim,na.value = 'transparent') 
    
pdf(file='Plot_rank_global.pdf')
plot_all_rank
dev.off()
