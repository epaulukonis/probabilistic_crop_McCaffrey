#Install and load supporting libraries.
start_time <- Sys.time()
print("in 00setup.R")

print(Sys.info()[4])
R.Version()$version.string

library(sp)
library(rgeos)
library(rgdal)
library(raster)
library(dplyr)
library(ggplot2)
#install.packages("disaggregation") ##for extracParrallel function
#library(disaggregation)
#install.packages('exactextractr')
#install.packages('exactextractr')
library(exactextractr) #requires geos

#who_is_running<-'eap'
who_is_running<-'stp'

if(Sys.info()[4]=="LZ2626UTPURUCKE"){
  root_dir <- file.path("c:", "git", "probabilistic_crop_McKaffrey")
}else if (Sys.info()[4]=="LZ26EPAULUKO"){
  #load('myEnvironment_prob_crop.RData')
  root_dir <- 'C:/Users/epauluko/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/GitHub/probabilistic_crop_McCaffrey'
}else{
  root_dir <- file.path("/work", "HONEYBEE", who_is_running, "probabilistic_crop_McCaffrey")
}
print(paste('root dir:', root_dir))

root_data_in <- file.path(root_dir, "data_in")
print(paste('root_data_in dir:', root_data_in))
root_data_out <- file.path(root_dir, "data_out")
print(paste('root_data_out dir:', root_data_out))
root_graphics <- file.path(root_dir, "graphics")
print(paste('root_graphics dir:', root_graphics))
root_src <- file.path(root_dir, "src")
print(paste('root_src dir:', root_src))


# data from ged
crop_data_dir1 = file.path(root_data_in, "FinalCropsPt1")
print(paste('crop_data_dir1:', crop_data_dir1))
crop_data_dir2 = file.path(root_data_in, "FinalCropsPt2")
print(paste('crop_data_dir2:', crop_data_dir2))
county_shp_dir = file.path(root_data_in, "cadwr_merced_shp")
print(paste('county_shp_dir:', county_shp_dir))

#unzip spatial files if necessary
#check if FinalCropsPt1 has been unzipped
unzip_dir1_filename <- file.path(root_data_in, "FinalCropsPt1.zip")
print(paste('unzip_dir1_filename:', unzip_dir1_filename))
check_dir1_filename <- file.path(crop_data_dir1, "Alfalfa_StudyArea.tif")
unzipped_dir1 <- file.exists(check_dir1_filename)
print(paste(check_dir1_filename, 'exists? =', unzipped_dir1))
if(unzipped_dir1){
  print(paste(check_dir1_filename, 'exists so no need to unzip'))
}else{
  unzip(unzip_dir1_filename, exdir=crop_data_dir1)
}
#check if FinalCropsPt2 has been unzipped
unzip_dir2_filename <- file.path(root_data_in, "FinalCropsPt2.zip")
check_dir2_filename <- file.path(crop_data_dir2, "Lettuce_StudyArea.tif")
print(unzip_dir2_filename)
unzipped_dir2 <- file.exists(file.path(crop_data_dir2, "Lettuce_StudyArea.tif"))
if(!unzipped_dir2){unzip(unzip_dir2_filename, exdir=crop_data_dir2)}
#check if county shapefiles has been unzipped
unzip_cadwr_filename <- file.path(root_data_in, "5_counties.zip")
print(unzip_cadwr_filename)
unzipped_cadwr <- file.exists(file.path(county_shp_dir, "cadwr_madera.shp"))
if(!unzipped_cadwr){unzip(unzip_cadwr_filename, exdir=county_shp_dir)}


#print current environment
print(lapply(ls(),function(x)get(x)))

#see how long initial setup takes
end_time <- Sys.time()
time_elapsed <- end_time - start_time
print(paste("time for initial setup:", time_elapsed))

# source other files
source(file.path(root_src, "01import_spatial_data.R"))
source(file.path(root_src, "02create_fields_level_pdfs.R"))
source(file.path(root_src, "03simulate_crop_field_assignments.R"))



