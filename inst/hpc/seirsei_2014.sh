#!/bin/bash

#SBATCH --job-name=run2014   

#SBATCH -N 1

#SBATCH -n 12  

#SBATCH -t 9:59:00  

#SBATCH --mem-per-cpu=80M    

module --ignore-cache load r/4.3.1		   
R CMD BATCH --no-save --no-restore seirsei_2014.txt