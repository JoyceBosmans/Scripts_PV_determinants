#!/bin/bash
#SBATCH --partition=milkun
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=joyce.bosmans@ru.nl
#SBATCH --time=30-00:00:00
#SBATCH --job-name=Determinants_PV_SAm
#SBATCH --output=slurm-%x.%j.out

#~ date +"%Y-%m-%d %T"
#~ echo '---input already prepared globally so create polygon mask, then get and check predictors'
#~ Rscript Script_create_country_polygons.R &
#~ wait

#~ date +"%Y-%m-%d %T"
#~ echo '---get locations of P and A, predictor values, including country name, see output_PA_predictors.txt'
#~ Rscript Script_PA_predictors.R > output_PA_predictors.txt &
#~ wait

#~ date +"%Y-%m-%d %T"
#~ echo '---check statistics, see output_predictor_stats.txt '
#~ Rscript Script_predictor_stats.R > output_predictor_stats.txt &
#~ wait

#~ date +"%Y-%m-%d %T"
#~ echo '---get full model (glmer), see output_glmer.txt'
#~ Rscript Script_glmer.R > output_glmer.txt &
#~ wait 

date +"%Y-%m-%d %T"
echo '---then check allFit (on full model) as well as dredge for the best model, see output_glmer_allfit.txt and output_find_best_glmer.txt'
#Rscript Script_glmer_allfit.R > output_glmer_allfit.txt &
Rscript Script_find_best_glmer.R > output_find_best_glmer.txt &
wait

#~ date +"%Y-%m-%d %T"
#~ echo '---then plot best model '
#~ Rscript Script_create_probability_map.R 
#~ wait
