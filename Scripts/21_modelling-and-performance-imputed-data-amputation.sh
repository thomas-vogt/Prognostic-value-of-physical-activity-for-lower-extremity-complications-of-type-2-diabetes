#!/bin/env bash

#SBATCH -A sens2020594                           
#SBATCH -p core
#SBATCH -n 5                        
#SBATCH -t 5-00:00:00

cd ../Processed_data/11_imputed-datasets

RANDOM=132
seeds_for_R=()
n_seeds=$(ls -l imputed*amputation* | wc -l)

for (( i=0; i<n_seeds; i=i+1 )); do
    seeds_for_R[i]="$RANDOM"
done

cd ../../Scripts

module load R_packages/4.2.1
module load gnuparallel/20230422

parallel --jobs 4 --verbose --link \
  'Rscript --no-save --no-restore 21_modelling-imputed-data.R {}' ::: ../Processed_data/11_imputed-datasets/imputed*amputat*.rds ::: "${seeds_for_R[@]}" 
