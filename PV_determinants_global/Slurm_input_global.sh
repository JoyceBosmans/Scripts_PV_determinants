#!/bin/bash
#SBATCH --partition=milkun
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=joyce.bosmans@ru.nl
#SBATCH --time=20-00:00:00
#SBATCH --job-name=split_global_input

# 18-10-21: add overlapping bits to avoid missing lats/lons for rsds

Rscript Script_input_global_split.R A -180 -119.5 29.5 90 & 
wait
Rscript Script_input_global_split.R B -120 -59.5 29.5 90 & 
wait
Rscript Script_input_global_split.R C -60 0.5 29.5 90 & 
wait
