
pacman::p_load(readxl,ggplot2, rgdal, spData, sp, sf, raster,writexl,data.table)

# P and A on a vector map (points or polygon)

df_PA <- read_xlsx('../PV_determinants_SAm/df_PA_SAm_country_060622.xlsx')
#colnames(df_PA)

xy   <- df_PA[,c('lon_orig','lat_orig')]
df_PA   <- df_PA[,c('lon_orig','lat_orig','PA')]

spdf <- SpatialPointsDataFrame(coords = xy, data = df_PA,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

typeof(spdf)

plot(spdf)

writeOGR(spdf,'spdf_SAm_PA',layer='PA',driver='ESRI Shapefile')


# then for presences only
nrow(df_PA)
df_P <- subset(df_PA, PA == 'P')
nrow(df_P)

xy   <- df_P[,c('lon_orig','lat_orig')]

spdf <- SpatialPointsDataFrame(coords = xy, data = df_P,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

plot(spdf)

writeOGR(spdf,'spdf_SAm_P',layer='P',driver='ESRI Shapefile')

