#!/bin/bash
#SBATCH --partition=milkun
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=joyce.bosmans@ru.nl
#SBATCH --time=20-00:00:00
#SBATCH --job-name=split_global_input_2

# 18-10-21: add overlapping bits to avoid missing lats/lons for rsds

Rscript Script_input_global_split.R J 0 60.5 -30.5 30 & 
wait
Rscript Script_input_global_split.R K 60 120.5 -30.5 30 & 
wait
Rscript Script_input_global_split.R L 120 180 -30.5 30 & 
wait

