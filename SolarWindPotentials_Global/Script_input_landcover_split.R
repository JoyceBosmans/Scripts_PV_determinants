pacman::p_load(raster)
args = commandArgs(trailingOnly=TRUE)
# args should be: area-letter lon-left lon-right lat-low lat-up

ESA_dir <- '/vol/milkundata/ESA_landcover/TIFF/'
year    <- 2000

outputfilename = paste0('ESA_stack_',year,'_',args[1],'.gri')
print(outputfilename)

ESA_map <- raster(paste0(ESA_dir,'ESACCI-LC-L4-LCCS-Map-300m-P1Y-',year,'-v2.0.7.tif'))

# group into 7 larger categories per 60x60 degree tile: 
extent_region <- extent(as.numeric(args[2]),as.numeric(args[3]),as.numeric(args[4]),as.numeric(args[5]))
ESA_grouped   <- crop(ESA_map,extent_region)
ESA_grouped[ESA_grouped %in% c(10,11,12,20,30,40)] <- 1
ESA_grouped[ESA_grouped %in% c(50,60,61,62,70,71,72,80,81,82,90,100,160,170)] <- 2
ESA_grouped[ESA_grouped %in% c(110,120,121,122,130,140,150,151,152,153)] <- 3
ESA_grouped[ESA_grouped %in% c(180)] <- 4
ESA_grouped[ESA_grouped %in% c(190)] <- 5
ESA_grouped[ESA_grouped %in% c(200,201,202)] <- 6
ESA_grouped[ESA_grouped %in% c(210,220)] <- 7


# split first, set occurence to 1, then aggregate by sum
aggr_fact = 4 	#aggregate to 4x4 cells, or from 0.00277 to 0.011 for ESA resolution
for(l in 1:7){
	print(paste('aggregate category'),l)
	temp <- ESA_grouped
	temp[temp != l] <- 0
	temp[temp == l] <- 1
	temp <- aggregate(temp,fact=aggr_fact,fun=sum)
	temp <- temp/(aggr_fact*aggr_fact)
	assign(paste0('ESA',l),temp)
} 

area_of_cells  <- area(ESA1)				#function area gives grid cell size in km2
available_area <- (1-ESA7)*area_of_cells	#exclude areas of water and ice (ESA classes 210, 220, see line 20)

# store this (as a raster stack)
ESA_stack <- stack(ESA1,ESA2,ESA3,ESA4,ESA5,ESA6,ESA7,available_area)
names(ESA_stack) <- c('Agriculture','Forest','Short_natural','Wetland','Urban','Bare','Water','available_area')
writeRaster(ESA_stack,filename=outputfilename,overwrite=TRUE,format='raster')

