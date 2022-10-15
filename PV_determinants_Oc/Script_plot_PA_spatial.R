pacman::p_load(readxl, writexl, ggplot2, raster,dismo,sf,countrycode)

df_PA <- read_xlsx('df_PA_Oc_country.xlsx')
df_PA$lon_new <- df_PA$lon_orig
#~ df_PA$lon_new[df_PA$lon_orig < -170] <- df_PA$lon_orig + 360

#~ df_PA$lon_new[df_PA$lon_orig == -171.7611] <- -171.7611 + 360

df_P <- df_PA[df_PA$PA == 'P',]
df_A <- df_PA[df_PA$PA == 'A',]

df_P <- df_P[c('lat_orig','lon_new')]
df_A <- df_A[c('lat_orig','lon_new')]

pdf(file='Plot_presences_absences_spatial.pdf')
ggplot() + theme_minimal() + 
geom_point(data=df_A,aes(x=lon_new,y=lat_orig),size=.5) +  geom_point(data=df_P, aes(x=lon_new, y=lat_orig),color='red',size=.5) + labs(title = paste0("Utility-scale PV presences (red, ", nrow(df_P),") and absences (black, ", nrow(df_A),")")) + coord_sf(xlim = c(100,180), ylim = c(-50,0), expand = FALSE) 
dev.off()

