extract_start_time <- Sys.time()
print("stepping into 02create_fields_level_pdfs.R")
print(Sys.time())

# crop_raster_stack was crop_stackf

##Step 2: create field level probabilities
#extract to fields
print("start fields extraction")
print("checking to see if spatial objects already exist")
extract_to_fields_filename <- file.path(root_data_out, "extract_to_fields.rds")
probs_by_fields_filename <- file.path(root_data_out, "probs_by_fields.rds")
out_filename <- file.path(root_data_out, "out.rds")
#function to convert the rasters to binary
fun_c <- function(x) {
  x[x>0] <- 1
  return(x)
}

if(file.exists(extract_to_fields_filename) && 
   file.exists(probs_by_fields_filename)&& 
   file.exists(out_filename)){
  # files exist so we will load the needed R objects
  print("loading extract_to_fields object")
  print(Sys.time())
  readRDS(file = extract_to_fields_filename)
  print("loading probs_by_fields object")
  print(Sys.time())
  readRDS(file = probs_by_fields_filename)
  print("loading out object")
  print(Sys.time())
  readRDS(file = out_filename)
}else{
  # files don't exist so we will create them
  print("extract the raster cell values by polygons to a df")
  extract_to_fields <- extract(crop_raster_stack, mercedf, df=T) 
  print(dim(extract_to_fields))
  print("summarize each crop by fields to mean")
  print(Sys.time())
  probs_by_fields <- extract_to_fields %>% group_by(ID) %>% summarise_all(funs(mean)) 
  print(dim(probs_by_fields))
  print("apply probs_by_fields to sum_c")
  print(Sys.time())
  sum_c <- apply(probs_by_fields[,c(2:30)], 1, sum) 
  print(dim(sum_c))
  print(Sys.time())
  probs_by_fields$NC <- round(1-sum_c,4) #add in column for non-crop
  print(dim(probs_by_fields))
  print("omit fields which don't overlap with crop data")
  probs_by_fields <- na.omit(probs_by_fields) 
  print(dim(probs_by_fields))
  out <- stack(calc(crop_raster_stack, fun_c)) #put that in a raster stack
  #save extract_to_fields and probs_by_fields
  # Save the objects to file
  print("saving extract_to_fields object")
  print(Sys.time())
  saveRDS(extract_to_fields, file = file.path(root_data_out, "extract_to_fields.rds"))
  print("saving probs_by_fields object")
  print(Sys.time())
  saveRDS(probs_by_fields, file = file.path(root_data_out, "probs_by_fields.rds"))
  print("saving out object")
  print(Sys.time())
  saveRDS(out, file = file.path(root_data_out, "out.rds"))
}
print("finished fields extraction")
extract_end_time <- Sys.time()
extract_time_elapsed <- extract_end_time - extract_start_time
print(paste("time for fields extraction:", extract_time_elapsed))
