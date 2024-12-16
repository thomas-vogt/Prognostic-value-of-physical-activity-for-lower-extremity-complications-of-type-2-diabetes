#!/bin/env bash  

#SBATCH -A sens2020594                           
#SBATCH -p core
#SBATCH -n 9                         
#SBATCH -t 12:00:00 


module load R_packages/4.2.1
module load gnuparallel/20230422

parallel --jobs 8 --verbose \
'Rscript --no-save --no-restore 12_imputation-majamp.R {}' ::: ../Processed_data/06_landmark-datasets/*.rds
