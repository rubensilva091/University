#!/bin/sh
#
#SBATCH --exclusive      # exclusive node for the job
#SBATCH --time=02:00     # allocation for 2 minutes

export OMP_NUM_THREADS=1
echo "Sequencial Running"
perf stat -e L1-dcache-load-misses,cache-misses,cache-references,instructions,cycles ./fluid_sim_seq

for i in {1..24}
do
    export OMP_NUM_THREADS=$i
    echo "Running with OMP_NUM_THREADS=$i"
    perf stat -e L1-dcache-load-misses,cache-misses,cache-references,instructions,cycles ./fluid_sim
done

