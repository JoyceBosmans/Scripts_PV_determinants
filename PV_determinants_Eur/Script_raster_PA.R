
pacman::p_load(readxl,ggplot2, rgdal, spData, sp, sf, raster,writexl,data.table)

# P and A on a vector map (points or polygon)

df_PA <- read_xlsx('df_PA_Eur_country_261121.xlsx')
#colnames(df_PA)

xy   <- df_PA[,c('lon_orig','lat_orig')]
df_PA   <- df_PA[,c('lon_orig','lat_orig','PA')]

spdf <- SpatialPointsDataFrame(coords = xy, data = df_PA,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

typeof(spdf)

pdf(file='spdf_Eur.pdf')
plot(spdf)
dev.off()

print('test1')
writeOGR(spdf,'spdf_Eur_PA',layer='PA',driver='ESRI Shapefile')
print('test2')

# then for presences only
nrow(df_PA)
df_P <- subset(df_PA, PA == 'P')
nrow(df_P)

xy   <- df_P[,c('lon_orig','lat_orig')]

spdf <- SpatialPointsDataFrame(coords = xy, data = df_P,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
pdf(file='spdf_Eur_P.pdf')
plot(spdf)
dev.off()

print('test1')
writeOGR(spdf,'spdf_Eur_P',layer='P',driver='ESRI Shapefile')
print('test2')
