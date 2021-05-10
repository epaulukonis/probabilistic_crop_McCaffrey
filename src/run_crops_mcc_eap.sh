#!/bin/csh

#SBATCH --job-name=CROPS_MCC
#SBATCH --ntasks=1
#SBATCH --partition=compute
#SBATCH --constraint=cascadelake
#SBATCH --time=99:00:00
#SBATCH --mail-user=paulukonis.elizabeth@epa.gov
#SBATCH --mail-type=BEGIN,END

setenv TMPDIR /work/HONEYBEE/eap/run_crops_mcc

module load intel/19.0.5
module load R/4.0.3
module load gcc/6.1.0
module load geos/3.8.1
module load gdal-3.1.3/intel-19.0
module load proj-7.1.1/intel-19.0
module load udunits-2.2.26/intel-19.0

Rscript  00setup.R
