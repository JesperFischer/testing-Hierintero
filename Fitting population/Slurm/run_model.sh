#!/bin/bash
#SBATCH --job-name=Hierarchical_Intero
#SBATCH --cpus-per-task=4
#SBATCH --mem=20000

Rscript run_model.R "$1"