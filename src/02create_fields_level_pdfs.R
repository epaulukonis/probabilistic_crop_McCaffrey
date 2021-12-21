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
print(dim(madera_shape))
#madera_raster_stack<-stack(crop_raster_stack2[1])
merced_shape<-do.call(rbind, counties_trans[2]) 
print(dim(merced_shape))
#merced_raster_stack<-stack(crop_raster_stack2[2])
sacramento_shape<-do.call(rbind, counties_trans[3]) 
print(dim(sacramento_shape))
#sacramento_raster_stack<-stack(crop_raster_stack2[3])
sanjoaquin_shape<-do.call(rbind, counties_trans[4]) 
print(dim(sanjoaquin_shape))
#sanjoaquin_raster_stack<-stack(crop_raster_stack2[4])
stanislaus_shape<-do.call(rbind, counties_trans[5]) 
print(dim(stanislaus_shape))
#stanislaus_raster_stack<-stack(crop_raster_stack2[5])

if( file.exists(madera_filename)&&
    file.exists(merced_filename)&&
    file.exists(sacramento_filename)&&
    file.exists(sanjoaquin_filename)&&
    file.exists(stanislaus_filename))
{
  # files exist so we will load the needed R objects
  print("files do exist, read them in")
  county<-"merced"
  print('specify county here')
  print(county)
  probs_by_fields<-readRDS(file = merced_filename)  #data frame of field probs by county ## change here!
  print(dim(probs_by_fields))
  print("loading extracted field file for county of interest here")
  print(Sys.time())
  
  if (county=='madera'){
    county_shape<-madera_shape
    #county_raster_stack<-madera_raster_stack
  } else if (county=='merced')
  {
    county_shape<-merced_shape
    #county_raster_stack<-merced_raster_stack
  }
  else if (county=='sacramento')
  {
    county_shape<-sacramento_shape
    #county_raster_stack<-sacramento_raster_stack
  }
  else if (county=='sanjoaquin')
  {
    county_shape<-sanjoaquin_shape
    #county_raster_stack<-sanjoaquin_raster_stack
  }
  else if (county=='stanislaus')
  {
    county_shape<-stanislaus_shape
    #county_raster_stack<-stanislaus_raster_stack
  }
  else {
    print("no county specified")
  }
  
  print("summarize each crop by fields to mean")
  field_areas<- as.data.frame(area(county_shape), na.rm=T) #area of each field in meters^2
  colnames(field_areas)[1]<-'field_areas'
  field_areas$ID<-1:nrow(probs_by_fields)  
  print(dim(field_areas))
  print("area of individual fields put into dataframe for 03 code")
  
  print("apply probs_by_fields to sum_c")
  print(dim(probs_by_fields))
  sum_c <- apply(probs_by_fields[,c(1:29)], 1, sum) 
  print(head(sum_c))
  probs_by_fields$NC <- round((1-sum_c),4)#add in column for non-crop
  print(dim(probs_by_fields))
  print("omit fields which don't overlap with crop data")
  probs_by_fields <- na.omit(probs_by_fields) 
  print(dim(probs_by_fields))
  probs_by_fields[,1:30] <-  round(probs_by_fields[,1:30],4)
  print(dim(probs_by_fields))
  probs_by_fields$ID<-1:nrow(probs_by_fields)
  probs_by_fields<-probs_by_fields[,c(31,1:30)]
  print(dim(probs_by_fields))
  
  print("saving probs_by_fields object")
  print(Sys.time())
  saveRDS(probs_by_fields, file = file.path(root_data_out, "probs_by_fields_mer.rds"))
  write.csv(probs_by_fields, file = file.path(root_data_out, "probs_by_fields_mer.csv"))

}else{
  # files don't exist so we will create them
  print("files don't exist, make them")
  print("extract the raster cell values by polygons to a df, double check that the masked rasterstack is there")
  print(file.exists(file.path(root_data_out, "crop_rasterstack_masked.rds")))
  print(dim(crop_raster_stack2[1]))
  print('check raster stack')
  print(dim(counties_trans[1]))
  print('check counties_trans')

  print(dim(stack(crop_raster_stack2[1])))
  print('stack looks in order?')
  print(dim(madera_shape))
  
  madera<- exact_extract(stack(crop_raster_stack2[1]), madera_shape, 'mean')
  names(madera)<-names(crop_raster_stack)
  merced <- exact_extract(stack(crop_raster_stack2[2]), merced_shape, 'mean')
  names(merced)<-names(crop_raster_stack)
  sacramento <- exact_extract(stack(crop_raster_stack2[3]), sacramento_shape, 'mean')
  names(sacramento)<-names(crop_raster_stack)
  sanjoaquin <- exact_extract(stack(crop_raster_stack2[4]), sanjoaquin_shape, 'mean')
  names(sanjoaquin)<-names(crop_raster_stack)
  stanislaus <- exact_extract(stack(crop_raster_stack2[5]), stanislaus_shape, 'mean')
  names(stanislaus)<-names(crop_raster_stack)
  print(Sys.time())
  
  #save to RDS files
  saveRDS(madera, file = file.path(root_data_out, "madera.rds"))
  saveRDS(merced, file = file.path(root_data_out, "merced.rds"))
  saveRDS(sacramento, file = file.path(root_data_out, "sacramento.rds"))
  saveRDS(sanjoaquin, file = file.path(root_data_out, "sanjoaquin.rds"))
  saveRDS(stanislaus, file = file.path(root_data_out, "stanislaus.rds"))
  
  county<-"merced"
  print('specify county here')
  print(county)
  probs_by_fields<-readRDS(file = merced_filename) #use extracted finished dataframe ## specify county here too
  print("loading extracted field file for county of interest here")
  print(Sys.time())
  
  if (county=='madera'){
    county_shape<-madera_shape
    #county_raster_stack<-madera_raster_stack
  } else if (county=='merced')
  {
    county_shape<-merced_shape
    #county_raster_stack<-merced_raster_stack
  }
  else if (county=='sacramento')
  {
    county_shape<-sacramento_shape
    #county_raster_stack<-sacramento_raster_stack
  }
  else if (county=='sanjoaquin')
  {
    county_shape<-sanjoaquin_shape
    #county_raster_stack<-sanjoaquin_raster_stack
  }
  else if (county=='stanislaus')
  {
    county_shape<-stanislaus_shape
    #county_raster_stack<-stanislaus_raster_stack
  }
  else {
    print("no county specified")
  }
  print("summarize each crop by fields to mean")
  field_areas<- as.data.frame(area(county_shape)) #area of each field in meters
  colnames(field_areas)[1]<-'field_areas'
  field_areas$ID<-1:nrow(probs_by_fields)  
  print(dim(field_areas))
  print("area of individual fields put into dataframe for 03 code")
  
  print(dim(probs_by_fields))
  print("apply probs_by_fields to sum_c")
  sum_c <- apply(probs_by_fields[,c(1:29)], 1, sum) 
  print(dim(sum_c))
  probs_by_fields$NC <- round((1-sum_c),4)#add in column for non-crop
  print(dim(probs_by_fields))
  print("omit fields which don't overlap with crop data")
  probs_by_fields <- na.omit(probs_by_fields) 
  print(dim(probs_by_fields))
  probs_by_fields$ID<-1:nrow(probs_by_fields)
  print(dim(probs_by_fields))
  probs_by_fields <- as.data.frame(c(probs_by_fields[,31], round(probs_by_fields [,1:30],4)))
  print(dim(probs_by_fields))
  
  print("saving probs_by_fields object")
  print(Sys.time())
  saveRDS(probs_by_fields, file = file.path(root_data_out, "probs_by_fields_mer.rds"))
  write.csv(probs_by_fields, file = file.path(root_data_out, "probs_by_fields_mer.csv"))
  
}

print("finished fields extraction, finished preparing data for 03")
extract_end_time <- Sys.time()
extract_time_elapsed <- extract_end_time - extract_start_time
print(paste("time for fields extraction:", extract_time_elapsed))

