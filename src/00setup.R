

#Install and load supporting libraries.
print(Sys.info()[4])
R.Version()$version.string

library(sp)
library(rgeos)
library(rgdal)
library(raster)
library(dplyr)

if(Sys.info()[4]=="LZ2626UTPURUCKE"){
  root_dir <- file.path("c:", "git", "probabilistic_crop_McKaffrey")
}else if (Sys.info()[4]=="LZ26EPAULUKO"){
  #load('myEnvironment_prob_crop.RData')
  root_dir <- 'C:/Users/epauluko/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/GitHub/probabilistic_crop_McCaffrey'
}else{
  root_dir <- file.path("/work", "HONEYBEE", "stp", "probabilistic_crop_McCaffrey")
}

root_data_in <- file.path(root_dir, "data_in")
root_data_out <- file.path(root_dir, "data_out")
root_graphics <- file.path(root_dir, "graphics")
root_src <- file.path(root_dir, "src")

# data from ged
crop_data_dir1 = file.path(root_data_in, "FinalCropsPt1")
crop_data_dir2 = file.path(root_data_in,"FinalCropsPt2")
merced_shp_dir = file.path(root_data_in,"cadwr_merced_shp")

# source other files
source(file.path(root_src, "01import_spatial_data.R"))
source(file.path(root_src, "02simulate_crop_field_assignments.R"))

#print current environment
print(lapply(ls(),function(x)get(x)))
