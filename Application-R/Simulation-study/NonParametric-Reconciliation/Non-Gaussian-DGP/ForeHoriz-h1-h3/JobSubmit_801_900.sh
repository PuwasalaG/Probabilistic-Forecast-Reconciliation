#!/bin/env bash



#SBATCH --job-name=Hierarchy9
#SBATCH --time=100:00:00

#SBATCH --mem=8G

#SBATCH --mail-type=BEGIN,END,FAIL

#SBATCH --mail-user=puwasala.gamakumara@monash.edu

#SBATCH --partition=short,medium,batch



echo "Running on node `hostname`"

module load R/3.5.3

R --vanilla < Hierarchy-2_GumbelwithBeta_801_900.R