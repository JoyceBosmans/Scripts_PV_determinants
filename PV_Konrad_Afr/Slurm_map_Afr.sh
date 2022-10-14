#!/bin/bash
#SBATCH --partition=milkun
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=joyce.bosmans@ru.nl
#SBATCH --time=5-00:00:00
#SBATCH --job-name=map_Afr
#SBATCH --output=slurm-%x.%j.out
#SBATCH --nodelist=cn37

date +"%Y-%m-%d %T"
echo '---map Afr '
Rscript map_Afr_Joyce.R 
wait
