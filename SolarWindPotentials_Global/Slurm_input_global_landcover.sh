#!/bin/bash
#SBATCH --partition=milkun
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=joyce.bosmans@ru.nl
#SBATCH --time=4-00:00:00
#SBATCH --job-name=input_global_landcover

Rscript Script_input_landcover_split.R A -180 -120 30 90 & 
wait
Rscript Script_input_landcover_split.R B -120 -60 30 90 & 
wait
Rscript Script_input_landcover_split.R C -60 0 30 90 & 
wait
Rscript Script_input_landcover_split.R D 0 60 30 90 & 
wait
Rscript Script_input_landcover_split.R E 60 120 30 90 & 
wait
Rscript Script_input_landcover_split.R F 120 180 30 90 & 
wait

Rscript Script_input_landcover_split.R G -180 -120 -30 30 & 
wait
Rscript Script_input_landcover_split.R H -120 -60 -30 30 & 
wait
Rscript Script_input_landcover_split.R I -60 0 -30 30 & 
wait
Rscript Script_input_landcover_split.R J 0 60 -30 30 & 
wait
Rscript Script_input_landcover_split.R K 60 120 -30 30 & 
wait
Rscript Script_input_landcover_split.R L 120 180 -30 30 & 
wait

Rscript Script_input_landcover_split.R M -180 -120 -90 -30 & 
wait
Rscript Script_input_landcover_split.R N -120 -60 -90 -30 & 
wait
Rscript Script_input_landcover_split.R O -60 0 -90 -30 & 
wait
Rscript Script_input_landcover_split.R P 0 60 -90 -30 & 
wait
Rscript Script_input_landcover_split.R Q 60 120 -90 -30 & 
wait
Rscript Script_input_landcover_split.R R 120 180 -90 -30 & 
wait
