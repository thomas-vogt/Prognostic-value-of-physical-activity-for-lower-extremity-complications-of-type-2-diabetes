#!/bin/env bash

#SBATCH -A sens2020594                           
#SBATCH -p core
#SBATCH -n 5
#SBATCH -t 01:00:00    


module load R_packages/4.2.1
module load gnuparallel/20230422

parallel --jobs 4 --verbose \
  'Rscript --no-save --no-restore 06_landmarking.R {}' ::: ../Processed_data/05_*.parquet
