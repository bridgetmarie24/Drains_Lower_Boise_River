#!/bin/bash
#SBATCH -J CASC_script  # job name
#SBATCH --mail-type=ALL
#SBATCH --mail-user=bridgetbittmann@u.boisestate.edu # email when the job is done
#SBATCH -o log%j  # output and error file name (%j expands to jobID)
#SBATCH -n 48 # total number of tasks requested
#SBATCH -N 1 #number of nodes you want to run
#SBATCH -p bsudfq # queue (partition) -- defq, ipowerq, eduq, gpuq.
#SBATCH -t 1-12:00:00 # run time (d-hh:mm:ss)
ulimit -v unlimited
ulimit -s unlimited
ulimit -u 10000

# Load modules for script
module load gcc/8.2.0
module load gdal/gcc8/3.0.4
module load R/gcc8/4.1.1

Rscript ./mixed_model_borah.R



