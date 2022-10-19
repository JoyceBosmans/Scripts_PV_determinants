##### import packages, data #####

# load packages
pacman::p_load(readxl, rgdal)

df_PA <- read_xlsx('../data/xlsx/df_PA_SAm_country_060622.xlsx')

# coordinate transformation
d <- data.frame(lon=df_PA$lon_orig, lat=df_PA$lat_orig)
coordinates(d) <- c("lon", "lat")
proj4string(d) <- CRS("+init=epsg:4326") # WGS 84
CRS.new <- CRS("+init=esri:102015") # Lambert projection
d_transformed <- spTransform(d, CRS.new)
df_PA$x_coord <- d_transformed$lon
df_PA$y_coord <- d_transformed$lat

# log transformation for some variables with highly logarithmic distributions
df_PA$travel_log <- log10(df_PA$access + 1)
df_PA$slope_log  <- log10(df_PA$slope)
df_PA$grid_log <- log10(df_PA$grid_km + 1)
df_PA$elev_log   <- log10(df_PA$elev + abs(min(df_PA$elev))+1)
df_PA$road_log <- log10(df_PA$road_dist + 1)

# normalize variables
df_PA$travel_log <- (df_PA$travel_log - mean(df_PA$travel_log))/sd(df_PA$travel_log)
df_PA$slope_log <- (df_PA$slope_log - mean(df_PA$slope_log))/sd(df_PA$slope_log)
df_PA$grid_log <- (df_PA$grid_log - mean(df_PA$grid_log))/sd(df_PA$grid_log)
df_PA$elev_log <- (df_PA$elev_log - mean(df_PA$elev_log))/sd(df_PA$elev_log)
df_PA$road_log <- (df_PA$road_log - mean(df_PA$road_log))/sd(df_PA$road_log)
df_PA$rsds <- (df_PA$rsds - mean(df_PA$rsds))/sd(df_PA$rsds)
df_PA$protect <- (df_PA$protect - mean(df_PA$protect))/sd(df_PA$protect)
df_PA$Agriculture <- (df_PA$Agriculture - mean(df_PA$Agriculture))/sd(df_PA$Agriculture)
df_PA$Short_natural <- (df_PA$Short_natural - mean(df_PA$Short_natural))/sd(df_PA$Short_natural)
df_PA$Wetland <- (df_PA$Wetland - mean(df_PA$Wetland))/sd(df_PA$Wetland)
df_PA$Urban <- (df_PA$Urban - mean(df_PA$Urban))/sd(df_PA$Urban)
df_PA$Bare <- (df_PA$Bare - mean(df_PA$Bare))/sd(df_PA$Bare)
df_PA$Water <- (df_PA$Water - mean(df_PA$Water))/sd(df_PA$Water)

# select variables
df_PA <- df_PA[,c('PA_bool', 'SOVEREIGN1', 'travel_log', 'slope_log', 'elev_log', 'road_log', 'grid_log', 'rsds', 'protect', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water', 'x_coord', 'y_coord')]

# save dataframe
save(df_PA, file = '../data/rda/df_PA_SAm_country_060622_proprocessed.Rda')
