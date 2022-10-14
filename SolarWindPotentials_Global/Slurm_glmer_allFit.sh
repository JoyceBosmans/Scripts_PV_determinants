#!/bin/bash
#SBATCH --partition=milkun
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=joyce.bosmans@ru.nl
#SBATCH --time=4-00:00:00
#SBATCH --job-name=glmer_allFit
#SBATCH -w cn37

R -e "rmarkdown::render('Report_glmer_allFit.Rmd',output_file='Report_glmer_allFit.html')"
