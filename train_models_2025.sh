#!/bin/bash -l
#SBATCH --job-name=all_models
#SBATCH --account=dongelr1
#SBATCH --output=output_hyy_bei20250519.txt
#SBATCH --error=errors_hyy_bei20250519.txt
#SBATCH --partition=small
#SBATCH --time=05:00:00
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --mem-per-cpu=3GB
#SBATCH --cpus-per-task=10

# Load r-env
module load r-env

# Try to fix prediction library error
#export R_LIBS=/scratch/dongelr1/laantito

# Clean up .Renviron file in home directory
if test -f ~/.Renviron; then
    sed -i '/TMPDIR/d' ~/.Renviron
fi

# Specify a temp folder path
echo "TMPDIR=/scratch/dongelr1" >> ~/.Renviron

# Run the R script
srun apptainer_wrapper exec Rscript --no-save /scratch/dongelr1/susannar/kesa2024/models/common_model-2025.R
