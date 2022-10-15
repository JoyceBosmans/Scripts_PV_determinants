#!/bin/bash
#SBATCH --partition=milkun
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=joyce.bosmans@ru.nl
#SBATCH --time=7-00:00:00
#SBATCH --job-name=Determinants_PV_Asia
#SBATCH --output=slurm-%x.%j.out
#SBATCH --nodelist=cn36


date +"%Y-%m-%d %T"
echo '---create probability map '
Rscript Script_combine_models_for_probability_map.R 
wait

#~ date +"%Y-%m-%d %T"
#~ echo '---create df_full half '
#~ Rscript Script_create_probability_maps_half.R 
#~ wait

#~ date +"%Y-%m-%d %T"
#~ echo '---check df_full '
#~ Rscript Script_create_probability_maps_check.R 
#~ wait
