#!/bin/bash
#SBATCH --partition=milkun
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=joyce.bosmans@ru.nl
#SBATCH --time=2-00:00:00
#SBATCH --job-name=input_solarwindpotentials

# 18-10-21: add overlapping bits to avoid missing lats/lons for rsds

Rscript Script_input_global_split.R A -180 -119.5 29.5 90 & 
wait
Rscript Script_input_global_split.R B -120 -59.5 29.5 90 & 
wait
Rscript Script_input_global_split.R C -60 0.5 29.5 90 & 
wait
Rscript Script_input_global_split.R D 0 60.5 29.5 90 & 
wait
Rscript Script_input_global_split.R E 60 120.5 29.5 90 & 
wait
Rscript Script_input_global_split.R F 120 180 29.5 90 & 
wait

Rscript Script_input_global_split.R G -180 -119.5 -30.5 30 & 
wait
Rscript Script_input_global_split.R H -120 -59.5 -30.5 30 & 
wait
Rscript Script_input_global_split.R I -60 0.5 -30.5 30 & 
wait
Rscript Script_input_global_split.R J 0 60.5 -30.5 30 & 
wait
Rscript Script_input_global_split.R K 60 120.5 -30.5 30 & 
wait
Rscript Script_input_global_split.R L 120 180 -30.5 30 & 
wait

Rscript Script_input_global_split.R M -180 -119.5 -90 -30 & 
wait
Rscript Script_input_global_split.R N -120 -59.5 -90 -30 & 
wait
Rscript Script_input_global_split.R O -60 0.5 -90 -30 & 
wait
Rscript Script_input_global_split.R P 0 60.5 -90 -30 & 
wait
Rscript Script_input_global_split.R Q 60 120.5 -90 -30 & 
wait
Rscript Script_input_global_split.R R 120 180.5 -90 -30 & 
wait
