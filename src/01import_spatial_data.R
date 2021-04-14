##Step 1: extract the spatial data (note that if you load the environment this work has been done for you)
#read in list of tif files denoting probability of each crop

crop_stack1_files <- file.path(crop_data_dir1, 
                         list.files(path=crop_data_dir1, pattern='.tif$', all.files=TRUE, full.names=FALSE))
crop_stack2_files <- file.path(crop_data_dir2, 
                       list.files(path=crop_data_dir2, pattern='.tif$', all.files=TRUE, full.names=FALSE))
crop_stack_allfiles <- c(crop_stack1_files, crop_stack2_files) #was #crop_stackf

crop_raster_stack <- stack(crop_stack_allfiles)

#read in clip of merced county fields
merced_shp_file <- file.path(merced_shp_dir, "cadwr_merced.shp")
merced <- readOGR(dsn = merced_shp_file)
plot(merced) #quick peek at the field layer
crs(merced)
crs(crop_raster_stack)
mercedf <- spTransform(merced,crs(crop_raster_stack)) #transform crs of merced polygon
ex <- extent(mercedf) #clip to extent of merced

plot(crop_raster_stack[[5]]) 
plot(mercedf, add=T)   # check to make sure that the merced plot is there

crop_raster_stack2 <-crop(crop_raster_stack, ex) #crop the crop stack to the merced polygon
extent(crop_raster_stack2) #check that extents match
extent(mercedf)
crop_raster_stack2<-stack(crop_raster_stack2) #turn it back into a stack
plot(crop_raster_stack2[[2]])
