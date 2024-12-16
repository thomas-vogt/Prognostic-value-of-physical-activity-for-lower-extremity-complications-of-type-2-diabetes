#!/bin/env bash

#SBATCH -A sens2020594                           
#SBATCH -p node
#SBATCH -N 1                          
#SBATCH -C mem256GB 
#SBATCH -t 96:00:00       


module load R_packages/4.1.1
module load gnuparallel/20230422

parallel --jobs 7 --verbose 'Rscript --no-save --no-restore {}' ::: 01_extract-*.R 
