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
module load R/3.6.2
module load geos/3.8.0
module load gdal-2.4.3/intel-19.0
module load proj-5.2.0/intel-19.0
module load udunits-2.2.26/intel-19.0



Rscript  00setup.R
