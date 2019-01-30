#!/bin/sh
#SBATCH -a 1-200
#SBATCH --cpus-per-task=1
#SBATCH --time=00:15:00
#SBATCH --mem=1G
#SBATCH -p sched_mit_sloan_batch
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=adelarue@mit.edu
#SBATCH --output=logs/experiment_\%a.log
module load sloan/julia/1.0.0
module load sloan/python/modules/2.7
srun julia analysis-complete.jl $SLURM_ARRAY_TASK_ID