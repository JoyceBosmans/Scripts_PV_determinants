Steps taken to predict spatial probability for solar parks. 

1. Get the input data (predictors) on the right resolution for the right region
--- predictors such as road distance from Mirza: Script_input_global_split.R, Slurm_input_global.sh
    result: pred_stack_global.gri
--- land cover types from ESA, grouped into fractions of 8 types: Script_input_landcover_split.R, Slurm_input_global_landcover.sh
    result: ESA_stack_2000.gri

2. Select presences and absences, create dataframe
--- presences from WikiSolar, absences from remaining (non-water) grid cells
    get predictor values, for all locations: Script_PA_predictors.R
    result: df_PA_global.csv (some data frame) 
--- add country names: Script_PA_add_country.R
    result: df_PA_global_country.csv

3. Check the predictors
--- distribution, multicollinearity, vifs: Script_predictor_stats.R
    result: which predictors to include and, if necessary, log-transform
    (exclude forest lc type, log-transform travel-time, road-distance etc)
    
4. Build the model
--- general linear mixed effect model, INLA, with country as random effect: Script_glmer.R
    Step 1: Fit a model on all data, do dredging based on WAIC, find best model
    Step 2: Validate on all data, get TSS, AUC, mR2 and cR2. 
	performed by Konrad
    
5. Generate probability map
--- apply INLA models to all grid points
--- then compare to IMAGE COE

-------------------------------------------
power lines: MV and HV
get slope from predictor_stack_global.gri
then: gdal_rasterize -burn 255 -l grid grid.gpkg slope.tif
then open slope.tif in R: grid[grid != 255] <- 0, grid[grid == 255] <- 1
then compute distance to grid:
gdal_proximity grid.tif grid_proximity.tif -distunits GEO (should take proximity to all non-zero values, hence all grids)

