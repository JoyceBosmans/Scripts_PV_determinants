#!/bin/bash
#SBATCH --partition=milkun
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=joyce.bosmans@ru.nl
#SBATCH --time=21-00:00:00
#SBATCH --job-name=glmer_renewable

Rscript Script_find_best_glmer.R &

wait
