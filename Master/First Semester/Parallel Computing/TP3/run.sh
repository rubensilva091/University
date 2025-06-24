#!/bin/bash
#SBATCH --time=2:00
#SBATCH --partition=cpar
#SBATCH --exclusive
#SBATCH --constraint=k20
#SBATCH --cpus-per-task=40

./bin/sim_fluid
