#!/bin/bash
#SBATCH --partition=milkun
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=joyce.bosmans@ru.nl
#SBATCH --time=5-00:00:00
#SBATCH --job-name=map_Oc
#SBATCH --output=slurm-%x.%j.out
#SBATCH --nodelist=cn15

date +"%Y-%m-%d %T"
echo '---map Oceania '
Rscript map_Oc_Joyce.R 
wait
