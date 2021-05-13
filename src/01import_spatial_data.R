import_start_time <- Sys.time()
print("stepping into 01import_spatial_data.R")

##Step 1: extract the spatial data (note that if you load the environment this work has been done for you)
#read in list of tif files denoting probability of each crop

print(list.files(path=crop_data_dir1, pattern='.tif$', all.files=TRUE, full.names=FALSE))
crop_stack1_files <- file.path(crop_data_dir1, 
                         list.files(path=crop_data_dir1, pattern='.tif$', all.files=TRUE, full.names=FALSE))
print(list.files(path=crop_data_dir2, pattern='.tif$', all.files=TRUE, full.names=FALSE))
crop_stack2_files <- file.path(crop_data_dir2, 
                       list.files(path=crop_data_dir2, pattern='.tif$', all.files=TRUE, full.names=FALSE))
crop_stack_allfiles <- c(crop_stack1_files, crop_stack2_files) #was #crop_stackf

# crop_raster_stack was crop_stackf
crop_raster_stack <- stack(crop_stack_allfiles)
print("raster data extracted")
county_names<-c('madera','merced','sacramento','sanjoaquin','stanislaus')
counties <- list.files(county_shp_dir, pattern="\\.shp$", full.names=TRUE)
counties <- lapply(counties, shapefile)
names(counties)<-county_names
print("county shapefiles extracted")
counties_trans <- lapply(counties, function(x) spTransform(x,crs(crop_raster_stack)))#transform crs of county polygon
ex <- lapply(counties_trans, function (x) extent(x)) #get extents of each county
crop_raster_stack2 <- lapply(ex, function(x) crop(crop_raster_stack, x)) #crop raster stack to each county
# crop_raster_stack2 <- lapply(counties_trans, function(x) mask(crop_raster_stack, x)) 

#check if masked raster stack already exists
print('check if the cropped and masked rasterstack exists, if not, run extract')
cropped_raster_stack_filename <- file.path(root_data_out,"crop_rasterstack_masked.rds")
cropped_raster_stack <- file.exists(cropped_raster_stack_filename)
print(cropped_raster_stack)
if(!cropped_raster_stack){crop_raster_stack2 <- lapply(counties_trans, function(x) mask(crop_raster_stack, x))
}else if (cropped_raster_stack){
  crop_raster_stack2 <- readRDS(cropped_raster_stack_filename)
} 

print('county shapefiles cropped to extent each county, put in list')
print('make sure each list element is a stack of rasters')
crop_raster_stack2<-lapply(crop_raster_stack2, stack)
saveRDS(crop_raster_stack2, file = file.path(root_data_out, "crop_rasterstack_masked.rds"))
print('raster stack masked and saved locally')
import_end_time <- Sys.time()
import_time_elapsed <- import_end_time - import_start_time
print(paste("time for importing spatial data:", import_time_elapsed))

