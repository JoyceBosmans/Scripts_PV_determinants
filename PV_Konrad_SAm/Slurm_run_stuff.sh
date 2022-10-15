#!/bin/bash
#SBATCH --partition=milkun
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=joyce.bosmans@ru.nl
#SBATCH --time=7-00:00:00
#SBATCH --job-name=Determinants_PV_SAm
#SBATCH --output=slurm-%x.%j.out


date +"%Y-%m-%d %T"
echo '---create probability map '
Rscript Script_combine_models_for_probability_map.R 
wait
