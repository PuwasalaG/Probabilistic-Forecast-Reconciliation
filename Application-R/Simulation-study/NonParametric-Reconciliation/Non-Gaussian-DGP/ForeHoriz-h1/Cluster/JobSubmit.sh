#!/bin/env bash

#SBATCH --job-name=Hierarchy

#SBATCH --time=70:00:00
#SBATCH --mem=8G
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=puwasala.gamakumara@monash.edu
#SBATCH --partition=short,medium,batch

echo "Running on node `hostname`"
module load R/3.4.4
R --vanilla < Hierarchy_2_GumbelwithBeta_751_1000.R