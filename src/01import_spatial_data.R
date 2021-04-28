import_start_time <- Sys.time()
print("stepping into 01import_spatial_data.R")

##Step 1: extract the spatial data (note that if you load the environment this work has been done for you)
#read in list of tif files denoting probability of each crop

crop_stack1_files <- file.path(crop_data_dir1, 
                         list.files(path=crop_data_dir1, pattern='.tif$', all.files=TRUE, full.names=FALSE))
crop_stack2_files <- file.path(crop_data_dir2, 
                       list.files(path=crop_data_dir2, pattern='.tif$', all.files=TRUE, full.names=FALSE))
crop_stack_allfiles <- c(crop_stack1_files, crop_stack2_files) #was #crop_stackf

# crop_raster_stack was crop_stackf
crop_raster_stack <- stack(crop_stack_allfiles)
print("spatial data extracted")

#read in clips of county files
merced_shp_file <- file.path(merced_shp_dir, "cadwr_merced.shp")
merced <- readOGR(dsn = county_shp_dir)

names<-c('madera','merced','sacramento','sanjoaquin','stanislaus')
counties <- list.files(county_shp_dir, pattern="\\.shp$", full.names=TRUE)
counties <- lapply(counties, shapefile)
names(counties)<-names
counties_trans <- lapply(counties, function(x) spTransform(x,crs(crop_raster_stack))) #transform crs of county polygon
ex <- lapply(counties_trans, function (x) extent(x)) 
crop_raster_stack2 <- lapply(ex, function(x) crop(crop_raster_stack, x))

extent(crop_raster_stack2[[1]])

counties[i] <- spTransform(counties[i],crs(crop_raster_stack)) #transform crs of merced polygon
ex[i] <- extent(myfiles[i]) 
#clip to extent of merced
# crop_raster_stack2 <-crop(crop_raster_stack, ex) #crop the crop stack to the merced polygon
# extent(crop_raster_stack2) #check that extents match
#turn it back into a stack
#plot(crop_raster_stack2[[2]])
county<-mercedf
print("merced clipped")
print("reassembled into a stack")
print("renamed to county for extract function and 02 code")

import_end_time <- Sys.time()
import_time_elapsed <- import_end_time - import_start_time
print(paste("time for importing spatial data:", import_time_elapsed))
