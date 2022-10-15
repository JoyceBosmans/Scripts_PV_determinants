#!/bin/bash
#SBATCH --partition=milkun
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=joyce.bosmans@ru.nl
#SBATCH --time=20-00:00:00
#SBATCH --job-name=split_global_input_2

# 18-10-21: add overlapping bits to avoid missing lats/lons for rsds

Rscript Script_input_global_split.R M -180 -119.5 -90 -30 & 
wait
Rscript Script_input_global_split.R N -120 -59.5 -90 -30 & 
wait
Rscript Script_input_global_split.R O -60 0.5 -90 -30 & 
wait
