#!/bin/env bash  

#SBATCH -A sens2020594                           
#SBATCH -p core
#SBATCH -n 16                         
#SBATCH -t 24:00:00 

module load R_packages/4.2.1
module load gnuparallel/20230422

parallel --jobs 16 --verbose \
  'Rscript --no-save --no-restore 21_modelling-complete-case.R {}' ::: ../Processed_data/06_landmark-datasets/*.rds
