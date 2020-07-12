#! /bin/bash
#SBATCH --job-name=AP_RecoFC
#SBATCH --output=trace_%a.out
#SBATCH --error=error_%a.err
#SBATCH --mem-per-cpu=4096
#SBATCH --time=24:00:00
#SBATCH --array=261-262
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --export=NONE
module load R/3.6.2-mkl
Rscript reconcile_forecasts.R $SLURM_ARRAY_TASK_ID 
