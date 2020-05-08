#! /bin/bash
#SBATCH --job-name=AP_BaseFC
#SBATCH --output=array_%A_%a.out
#SBATCH --error=array_%A_%a.err
#SBATCH --array=1-8
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --export=NONE
module load R/3.6.2-mkl
Rscript base_forecasts.R $SLURM_ARRAY_TASK_ID 
