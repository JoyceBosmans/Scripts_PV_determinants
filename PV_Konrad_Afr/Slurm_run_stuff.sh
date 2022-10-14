#!/bin/bash
#SBATCH --partition=milkun
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=joyce.bosmans@ru.nl
#SBATCH --time=3-00:00:00
#SBATCH --job-name=Combine_Afr
#SBATCH --output=slurm-%x.%j.out
#SBATCH --nodelist=cn36

date +"%Y-%m-%d %T"
echo '---combine output and create full map '
Rscript Script_combine_models_for_probability_map2.R 
wait
