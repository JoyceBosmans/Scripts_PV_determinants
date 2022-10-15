pacman::p_load(readxl,ggplot2, writexl)

# get file with P and A locations
input_xls  <- 'df_PA_Eur_country_261121.xlsx'
output_xls <- 'df_PA_Eur_weightdist_261121.xlsx'

df_PA <- read_excel(input_xls)
PA    <- nrow(df_PA)
df_P  <- df_PA[df_PA$PA == 'P',]
P     <- nrow(df_P)

# get output from Jelle's computations of distances of all PA to all P (in meters)
dir_Jelle   <- '/vol/milkunA/jhilbers/Main_folders/Jelle/Code_advies/Joyce_2022'
file_Jelle  <- 'distance_matrix.csv'
dist_matrix <- read.csv(file.path(dir_Jelle,file_Jelle))

# check that nrow(dist_matrix) is #P * #PA
print(paste('# of entrences in dist_matrix:',nrow(dist_matrix)))
print(paste('# PA:',PA, '# P:', P, 'total:', PA*P))

# loop through dist_matrix (list of all PA points distance to all P) and create list of weighted distances with length PA
weighted_dist <- rep(NA,PA)
i = 1	# looping over all P
j = 1   # looping over all PA
for(j in 1:PA){
  sum <- 0
  for(i in 1:P){
	i <- i+P*(j-1)	# shift to get the chunks of length P
    inv_dist <- 1/(dist_matrix$dist[i]/1000)	# distance from m to km
    if (inv_dist != Inf){
      sum <- sum + inv_dist}}
  weighted_dist[j] <- sum
  #print(paste(j,sum))
}

df_PA$weighted_dist <- weighted_dist
# for plotting / checking purposes:
df_PA$weighted_dist_cutoff <- df_PA$weighted_dist
df_PA$weighted_dist_cutoff[df_PA$weighted_dist_cutoff > 5] <- 5
ggplot() + theme_minimal() + geom_point(data=df_PA,aes(x=lon_orig,y=lat_orig,color=weighted_dist_cutoff,shape=PA),show.legend=TRUE,size=0.05) +scale_color_gradientn(colours = rainbow(6))

write_xlsx(df_PA,output_xls)
