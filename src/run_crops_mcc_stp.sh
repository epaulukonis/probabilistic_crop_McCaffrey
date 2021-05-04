#!/bin/csh

#SBATCH --job-name=CROPS_MCC
#SBATCH --ntasks=1
#SBATCH --partition=compute
#SBATCH --constraint=cascadelake
#SBATCH --time=99:00:00
#SBATCH --mail-user=purucker.tom@epa.gov
#SBATCH --mail-type=BEGIN,END

setenv TMPDIR /work/HONEYBEE/stp/run_crops_mcc

module load intel/19.0
module load R/4.0.3
module load geos/3.8.1
module load gdal-3.1.3/intel-19.0
module load proj-7.1.1/intel-19.0
module load udunits-2.2.28/intel-19.0



Rscript  00setup.R
