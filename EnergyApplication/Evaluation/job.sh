#! /bin/bash
#SBATCH --job-name=AP_Energy
#SBATCH --output=trace_%a.out
#SBATCH --error=error_%a.err
#SBATCH --mem-per-cpu=4096
#SBATCH --time=24:00:00
#SBATCH --array=196-365
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --export=NONE
module load R/3.6.2-mkl
Rscript base_forecasts.R $SLURM_ARRAY_TASK_ID 
