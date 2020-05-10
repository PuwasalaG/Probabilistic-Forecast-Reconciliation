#! /bin/bash
#SBATCH --job-name=AP_RecoFC
#SBATCH --output=array_%A_%a.out
#SBATCH --error=array_%A_%a.err
#SBATCH --mem-per-cpu=8192
#SBATCH --array=1-32
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --export=NONE
module load R/3.6.2-mkl
Rscript reconcile_forecasts.R $SLURM_ARRAY_TASK_ID 
