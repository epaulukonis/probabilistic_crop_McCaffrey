extract_start_time <- Sys.time()
print("stepping into 02create_fields_level_pdfs.R")
print(Sys.time())

#Step 2: extract to fields
print("start loading extracted df or conduct fields extraction")
madera_filename <- file.path(root_data_out, "madera.rds")
merced_filename <- file.path(root_data_out, "merced.rds")
sacramento_filename <- file.path(root_data_out, "sacramento.rds")
sanjoaquin_filename <- file.path(root_data_out, "sanjoaquin.rds")
stanislaus_filename <- file.path(root_data_out, "stanislaus.rds")

#load county shapes and raster stacks
madera_shape<-do.call(rbind, counties_trans[1]) 
madera_raster_stack<-stack(crop_raster_stack2[1])
merced_shape<-do.call(rbind, counties_trans[2]) 
merced_raster_stack<-stack(crop_raster_stack2[2])
sacramento_shape<-do.call(rbind, counties_trans[3]) 
sacramento_raster_stack<-stack(crop_raster_stack2[3])
sanjoaquin_shape<-do.call(rbind, counties_trans[4]) 
sanjoaquin_raster_stack<-stack(crop_raster_stack2[4])
stanislaus_shape<-do.call(rbind, counties_trans[5]) 
stanislaus_raster_stack<-stack(crop_raster_stack2[5])

if( file.exists(madera_filename)&&
    file.exists(merced_filename)&&
    file.exists(sacramento_filename)&&
    file.exists(sanjoaquin_filename)&&
    file.exists(stanislaus_filename))
  {
# files exist so we will load the needed R objects
county<-"madera"
print('specify county here')
probs_by_fields<-readRDS(file = madera_filename)  #dataframe of field probs by county
print("loading extracted field file for county of interest here")
print(Sys.time())

if (county=='madera'){
county_shape<-madera_shape
county_raster_stack<-madera_raster_stack
} else if (county=='merced')
{
county_shape<-merced_shape
county_raster_stack<-merced_raster_stack
}
else if (county=='sacramento')
{
  county_shape<-sacramento_shape
  county_raster_stack<-sacramento_raster_stack
}
else if (county=='sanjoaquin')
{
  county_shape<-sanjoaquin_shape
  county_raster_stack<-sanjoaquin_raster_stack
}
else if (county=='stanislaus')
{
  county_shape<-stanislaus_shape
  county_raster_stack<-stanislaus_raster_stack
}
else {
  print("no county specified")
}

print("summarize each crop by fields to mean")
field_areas<- as.data.frame(area(county_shape)) #area of each field in meters
colnames(field_areas)[1]<-'field_areas'
field_areas$ID<-1:nrow(probs_by_fields)  
print("area of individual fields put into dataframe for 03 code")

print("apply probs_by_fields to sum_c")
print(Sys.time())
sum_c <- apply(probs_by_fields[,c(1:29)], 1, sum) 
print(dim(sum_c))
print(Sys.time())
probs_by_fields$NC <- round((1-sum_c),4)#add in column for non-crop
print(dim(probs_by_fields))
print("omit fields which don't overlap with crop data")
probs_by_fields <- na.omit(probs_by_fields) 
probs_by_fields$ID<-1:nrow(probs_by_fields)
print(dim(probs_by_fields))
probs_by_fields <- as.data.frame(c(probs_by_fields[,31], round(probs_by_fields [,1:30],4)))

#function to convert the rasters to binary
# fun_c <- function(x) {
#   x[x>0] <- 1
#   return(x)
# }
# out <- stack(calc(county_raster_stack, fun_c)) #put that in a raster stack, make sure to specify raster stack layer
#save probs_by_fields and out
print("saving probs_by_fields object")
print(Sys.time())
saveRDS(probs_by_fields, file = file.path(root_data_out, "probs_by_fields.rds"))
# print("saving out object")
# print(Sys.time())
# saveRDS(out, file = file.path(root_data_out, "out.rds"))
}else{
  # files don't exist so we will create them
  print("extract the raster cell values by polygons to a df")
  madera<- exact_extract(stack(crop_raster_stack2[1]), do.call(rbind, counties_trans[1]), 'mean')
  names(madera)<-names(crop_raster_stack)
  merced <- exact_extract(stack(crop_raster_stack2[2]), do.call(rbind, counties_trans[2]), 'mean')
  names(merced)<-names(crop_raster_stack)
  sacramento <- exact_extract(stack(crop_raster_stack2[3]), do.call(rbind, counties_trans[3]), 'mean')
  names(sacramento)<-names(crop_raster_stack)
  sanjoaquin <- exact_extract(stack(crop_raster_stack2[4]), do.call(rbind, counties_trans[4]), 'mean')
  names(sanjoaquin)<-names(crop_raster_stack)
  stanislaus <- exact_extract(stack(crop_raster_stack2[5]), do.call(rbind, counties_trans[5]), 'mean')
  names(stanislaus)<-names(crop_raster_stack)
  print(Sys.time())

  #save to RDS files
  saveRDS(madera, file = file.path(root_data_out, "madera.rds"))
  saveRDS(merced, file = file.path(root_data_out, "merced.rds"))
  saveRDS(sacramento, file = file.path(root_data_out, "sacramento.rds"))
  saveRDS(sanjoaquin, file = file.path(root_data_out, "sanjoaquin.rds"))
  saveRDS(stanislaus, file = file.path(root_data_out, "stanislaus.rds"))
}

print("finished fields extraction, finished preparing data for 03")
extract_end_time <- Sys.time()
extract_time_elapsed <- extract_end_time - extract_start_time
print(paste("time for fields extraction:", extract_time_elapsed))


# # if(file.exists(probs_by_fields_filename)&& 
# #    file.exists(out_filename)){
# #   # files exist so we will load the needed R objects
# #   print("loading probs_by_fields object")
# #   print(Sys.time())
# #   readRDS(file = probs_by_fields_filename)
# #   print("loading out object")
# #   print(Sys.time())
# #   readRDS(file = out_filename)
# # }else{
# #   # files don't exist so we will create them
# #   print("extract the raster cell values by polygons to a df")
#   # madera<- exact_extract(stack(crop_raster_stack2[1]), do.call(rbind, counties_trans[1]), 'mean') 
#   # names(madera)<-names(crop_raster_stack)
#   # merced <- exact_extract(stack(crop_raster_stack2[2]), do.call(rbind, counties_trans[2]), 'mean') 
#   # names(merced)<-names(crop_raster_stack)
#   # sacramento <- exact_extract(stack(crop_raster_stack2[3]), do.call(rbind, counties_trans[3]), 'mean') 
#   # names(sacramento)<-names(crop_raster_stack)
#   # sanjoaquin <- exact_extract(stack(crop_raster_stack2[4]), do.call(rbind, counties_trans[4]), 'mean') 
#   # names(sanjoaquin)<-names(crop_raster_stack)
#   # stanislaus <- exact_extract(stack(crop_raster_stack2[5]), do.call(rbind, counties_trans[5]), 'mean') 
#   # names(stanislaus)<-names(crop_raster_stack)
#   # print(Sys.time())
# # }
#   #save to RDS files
#   # saveRDS(madera, file = file.path(root_data_out, "madera.rds"))
#   # saveRDS(merced, file = file.path(root_data_out, "merced.rds"))
#   # saveRDS(sacramento, file = file.path(root_data_out, "sacramento.rds"))
#   # saveRDS(sanjoaquin, file = file.path(root_data_out, "sanjoaquin.rds"))
#   # saveRDS(stanislaus, file = file.path(root_data_out, "stanislaus.rds"))
#   
#   #note: here is where we specify the county
#   #probs_by_fields is the extracted pixel to field dataframe