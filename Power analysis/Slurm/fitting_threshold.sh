#!/bin/bash
#SBATCH --job-name=Hierarchical_Intero_power
#SBATCH --cpus-per-task=1
#SBATCH --mem=2000

# Define your R script and its arguments
R_SCRIPT="fitting_threshold.R"
SUBJECTS="0"          # Set your default value for subjects
TRIALS="0"           # Set your default value for trials
EFFECT_SIZE="0"      # Set your default value for effect size

# If command line arguments are provided, use them
if [ "$#" -eq 3 ]; then
  SUBJECTS=$1
  TRIALS=$2
  EFFECT_SIZE=$3
fi

# Run R script with the specified arguments
Rscript $R_SCRIPT $SUBJECTS $TRIALS $EFFECT_SIZE