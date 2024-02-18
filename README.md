# Hierarchical Interoception
This Github repository is for the paper "Hierarchical Interoception" and contains analyses of a large cohort performing the Heart rate distrimination (HRD) task as well as power simulations using the population level estimates gained. The repository also contains code, models and examples of how external users of the HRD can refit their analyses using the Hierarchical models formulized.

## Table of Contents
1. [Introduction](#introduction)
2. [Directory Structure](#directory-structure)
3. [Usage](#usage)


## Introduction
This is all the scripts, models and data used for the Hierarchical interoception paper. 

For complete reproducibility the project is embedded with [OSF](). The computational environment can be reproduced using the [renv](https://rstudio.github.io/renv/articles/renv.html) package for R environments.


## Directory Structure

The repository is structured in the following way:

```         

├── README.md             # overview of the project.
│
├── Figures              # Figures generated from code for the main paper.
│   └── ... 
│
├── Fitting population          # Directory containing scripts and models to rerun the population level analysis
│   │
│   ├── R scripts                     # Directory of R scripts for loading and organizing the raw data
│   ├── Slurm                         # Directory of R and bash scripts that run the Stan models from the Stan models directory.
│   └── Stan models                   # Directory of directories of Stan models of the five different psychometric functions tested with and without prior specification
│
│
├── Power analysis              # Directory containing all scripts and models to rerun the power analysis.
│   │
│   ├── Data setes                                    # Directory of generated datasets from the poweranalysis (from PSI)
│   ├── Power analysis results                        # Directory of directories of results of the threshold, slope or both
│   ├── Python scripts                                # Directory of the python script used for getting trial by trial delta BPM values
│   ├── R scripts                                     # Directory of R scripts used to generate simulated datasets
│   ├── Slurm                                         # Directory of R and bash scripts to run batches of power analyses
│   ├── Stan models                                   # Directory of directories of Stan models used for either simulations or visualizations
│   └── Visualization.Rmd                             # R markdown used to generate plot for the manuscript from the poweranalysis
│
│
└── Fitting functions           # Directory containing scripts and models for running the hierarchical analysis with covariates
    │
    ├── Stan models                                   # Directory of directories of Stan models
    └── Fitting.Rmd                                   # R markdown used to show how to use the model

```

## Access

To get access to the repository users are recommended to clone the respository with the following command in the terminal

```bash
git clone  https://github.com/JesperFischer/Hierarchical-Interoception.git
```
