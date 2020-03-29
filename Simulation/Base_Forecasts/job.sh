#! /bin/bash
#SBATCH --job-name=AP_BaseFC
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --export=NONE
module load R/3.6.2-mkl
Rscript base_forecasts.R
