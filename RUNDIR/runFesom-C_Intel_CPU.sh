#!/bin/bash

# Run script to perform a single offline analysis with icono-pdaf

### Levante
#SBATCH --job-name=fesomC # Specify job name
#SBATCH --partition=compute    # Specify partition name
#SBATCH --nodes=1             # Specify number of nodes
#SBATCH --ntasks-per-node=128
#SBATCH --cpus-per-task=1    # Specify number of CPUs per task
#SBATCH --time=00:30:00        # Set a limit on the total run time
#SBATCH --output=my_job.o%j    # File name for standard output
#SBATCH --error=my_job.e%j     # File name for standard error output
#SBATCH --mem=0                # Request full memory of a node (needed for scalability tests)
#SBATCH --account=ka1298      # Charge resources on this project account

### Bind your OpenMP threads
export OMP_NUM_THREADS=1
export OMP_SCHEDULE="auto"
export KMP_AFFINITY="verbose,granularity=fine,scatter"
export KMP_LIBRARY="turnaround"

ulimit -s 204800
ulimit -c 0
export OMP_STACKSIZE=128M

export I_MPI_PMI=pmi
export I_MPI_PMI_LIBRARY=/usr/lib64/libpmi.so

####################################################################

set -x
module load intel-oneapi-compilers
module load intel-oneapi-mpi/2021.5.0-gcc-11.2.0

fesom_coast_exe='./fesom_coast.exe'

srun -l  --cpu_bind=verbose --cpus-per-task=${OMP_NUM_THREADS} --hint=nomultithread --distribution=block:cyclic:block \
            ${fesom_coast_exe} 1>out.fesom-c 2>err.fesom-c

