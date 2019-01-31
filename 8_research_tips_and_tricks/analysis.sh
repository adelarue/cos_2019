#!/bin/sh
#SBATCH -a 1-200
#SBATCH --cpus-per-task=1
#SBATCH --mem=1G
#SBATCH -p sched_mit_sloan_batch
#SBATCH --output=logs/experiment_\%a.log
#SBATCH --mail-user=adelarue@mit.edu
#SBATCH --mail-type=END,FAIL

module load sloan/julia/1.0.0
module load sloan/python/modules/2.7
srun julia analysis.jl $SLURM_ARRAY_TASK_ID