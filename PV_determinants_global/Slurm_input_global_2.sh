#!/bin/bash
#SBATCH --partition=milkun
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=joyce.bosmans@ru.nl
#SBATCH --time=20-00:00:00
#SBATCH --job-name=split_global_input

# 18-10-21: add overlapping bits to avoid missing lats/lons for rsds

Rscript Script_input_global_split.R D 0 60.5 29.5 90 & 
wait
Rscript Script_input_global_split.R E 60 120.5 29.5 90 & 
wait
Rscript Script_input_global_split.R F 120 180 29.5 90 & 
wait

