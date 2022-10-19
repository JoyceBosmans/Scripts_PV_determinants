##### import packages, data #####

# load packages
pacman::p_load(readxl, rgdal)

df_allgridcells <- '../../../../Mountpoint3/PV_Konrad_Eur/df_Eur_allgridcells.csv'

df_full <- read.csv(df_allgridcells)

# coordinate transformation
d <- data.frame(lon=df_full$lon_orig, lat=df_full$lat_orig)
coordinates(d) <- c("lon", "lat")
proj4string(d) <- CRS("+init=epsg:4326") # WGS 84
CRS.new <- CRS("+init=esri:102014") # Lambert projection
d_transformed <- spTransform(d, CRS.new)
df_full$x_coord <- d_transformed$lon
df_full$y_coord <- d_transformed$lat

# log transformation for some variables with highly logarithmic distributions
df_full$travel_log <- log10(df_full$access + 1)
df_full$slope_log  <- log10(df_full$slope)
df_full$elev_log   <- log10(df_full$elev + abs(min(df_full$elev))+1)
df_full$road_log <- log10(df_full$road_dist + 1)

# normalize variables
df_full$travel_log <- (df_full$travel_log - mean(df_full$travel_log))/sd(df_full$travel_log)
df_full$slope_log <- (df_full$slope_log - mean(df_full$slope_log))/sd(df_full$slope_log)
df_full$elev_log <- (df_full$elev_log - mean(df_full$elev_log))/sd(df_full$elev_log)
df_full$road_log <- (df_full$road_log - mean(df_full$road_log))/sd(df_full$road_log)
df_full$rsds <- (df_full$rsds - mean(df_full$rsds))/sd(df_full$rsds)
df_full$protect <- (df_full$protect - mean(df_full$protect))/sd(df_full$protect)
df_full$Agriculture <- (df_full$Agriculture - mean(df_full$Agriculture))/sd(df_full$Agriculture)
df_full$Short_natural <- (df_full$Short_natural - mean(df_full$Short_natural))/sd(df_full$Short_natural)
df_full$Wetland <- (df_full$Wetland - mean(df_full$Wetland))/sd(df_full$Wetland)
df_full$Urban <- (df_full$Urban - mean(df_full$Urban))/sd(df_full$Urban)
df_full$Bare <- (df_full$Bare - mean(df_full$Bare))/sd(df_full$Bare)
df_full$Water <- (df_full$Water - mean(df_full$Water))/sd(df_full$Water)

# select variables
df_full <- df_full[,c('PA_bool', 'SOVEREIGN1', 'travel_log', 'slope_log', 'elev_log', 'road_log', 'rsds', 'protect', 'Agriculture', 'Short_natural', 'Wetland', 'Urban', 'Bare', 'Water', 'x_coord', 'y_coord')]

# save dataframe
save(df_full, file = '../data/rda/df_full_Eur_country_060622_proprocessed.Rda')
