pacman::p_load(readxl,ggplot2, writexl)

### use Jelle's computation to create a distance matrix to use in moran's I computation
# old messy script. Implemented Jelle's distance computation in Script-autocor directly (result: same). 

# get file with P and A locations
input_xls  <- '../PV_determinants_SAm/df_PA_SAm_country_060622.xlsx'
output_xls <- 'distance_matrix_SAm_processed.csv'

df_PA <- read_excel(input_xls)
PA    <- nrow(df_PA)
df_P  <- df_PA[df_PA$PA == 'P',]
P     <- nrow(df_P)

# get output from Jelle's computations of distances of all PA to all P (in meters)
dir_Jelle   <- '/vol/milkunA/jhilbers/Main_folders/Jelle/Code_advies/Joyce_2022'
file_Jelle  <- 'distance_matrix_SAm.csv'
dist_matrix <- read.csv(file.path(dir_Jelle,file_Jelle))

# check that nrow(dist_matrix) is #PA * #PA
print(paste('# of entrences in dist_matrix:',nrow(dist_matrix)))
print(paste('# PA:',PA, 'total:', PA*PA))

# loop through dist_matrix (list of all PA points distance to all P) and create list of weighted distances with length PA
weighted_dist <- matrix(,nrow=PA,ncol=PA)
i = 1	# looping over all P
j = 1   # looping over all PA
for(j in 1:PA){
  #for(i in 1:PA){
   # print(paste(j,i,PA*(j-1))) 
	#i <- i+PA*(j-1)	# shift to get the chunks of length P
  #weighted_dist[j,] <- dist_matrix[j+(j-1)*PA:j+j*PA-1]
  print(paste(j,j+(j-1)*PA,j+j*PA-1))
  #print(paste(j,sum))
  if(j > 10){break}
}

write_csv(weighted_dist,output_csv)

# j* chunk of 1-PA
