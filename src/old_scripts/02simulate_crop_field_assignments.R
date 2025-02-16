start_time <- Sys.time()
print("stepping into 02simulate_crop_field_assignments.R")
# crop_raster_stack was crop_stackf

##Step 2: Simulate field level probabilities
#extract to fields
print("field extraction")
nsims <- 10
#begin/end cluster! do tomorrow 4/27
extract_to_fields<- extract(crop_raster_stack, mercedf, df=T) #this extracts the raster cell values by polygons to a df
probs_by_fields<-extract_to_fields %>% group_by(ID) %>% summarise_all(funs(mean)) #summarize each crop by field to mean
sum_c <- apply(probs_by_fields[,c(2:30)], 1, sum) 
probs_by_fields$NC<-round(1-sum_c,4) #add in column for non-crop
probs_by_fields<-na.omit(probs_by_fields) #omit fields which don't overlap with crop data
simulation_matrix<-as.data.frame(matrix(data=NA,nrow=nrow(probs_by_fields),ncol=nsims)) #set up empty df to hold simulations
colnames(simulation_matrix)[1:nsims]<-paste0("Sim",1:nsims,"")
ID<-unique(probs_by_fields$ID)
simulation_matrix<-cbind(ID,simulation_matrix)
#function to convert the rasters to binary
fun_c <- function(x) {
  x[x>0] <- 1
  return(x)
}
out<-stack(calc(crop_raster_stack, fun_c)) #put that in a raster stack
end_time <- Sys.time()
time_elapsed <- end_time - start_time
print(paste("time for field extraction:", time_elapsed))


print("calculating raster areas")
#calculate the area (m2) of each type of raster (crop/non-crop) for each crop type
m<-list()
for (i in 1:29) {
  y <- out[[i]]
  m[[i]]<-as.matrix(tapply(area(y), y[], sum))
}

m_a<-lapply(m, `length<-`, max(lengths(m))) #change length of list to all match
total_crop_and_field_area<- data.frame(matrix(unlist(m_a), nrow=length(m_a), byrow=TRUE)) #turn into workable dataframe for total crop area
NC<-total_crop_and_field_area[1,1] 
total_crop_and_field_area<-as.data.frame(total_crop_and_field_area[,2])
total_crop_and_field_area[30,]<-NC
colnames(total_crop_and_field_area)[1]<-'crop_total'
total_crop_and_field_area[is.na(total_crop_and_field_area)] <- 0 #add in 0 #this is the dataset containing the total crop areas for each crop
total_crop_and_field_area$field_tot<-0
total_crop_and_field_area<-as.data.frame(t(total_crop_and_field_area))
colnames(total_crop_and_field_area)<-colnames(probs_by_fields)[2:31] 

#dataset for updating the areas that matches ncol and nrow of simulation_matrix
area_by_field<-probs_by_fields[,2:31]
area_by_field[,1:30]<-0
area_by_field<-as.data.frame(area_by_field) 

##loaded environment contains everything above up to here
crop_props<-as.data.frame(matrix(data=0,nrow=3,ncol=30)) #empty data-frame for calculating new probabilities
crop_props[2,]<-total_crop_and_field_area[1,]
names(crop_props)<-names(probs_by_fields[,2:31])

#field loop
print("big loop over simulations and fields")
start_time <- Sys.time()
for (simulation in 2:nsims){ #1000  
  sim_start_time <- Sys.time()
  for (field in 1:nrow(simulation_matrix)){ #16000   
    out<-probs_by_fields[probs_by_fields$ID %in% simulation_matrix[field,1],]
    crop_props[1,]<-out[,2:31] #pull out crop probs
    new_crop_props<-(1-(crop_props[3,]/crop_props[2,]))*crop_props[1,] 
    new_crop_props<-rapply(new_crop_props, function(x) ifelse(is.nan(x),0,x), how="replace" ) 
    new_crop_props[new_crop_props < 0] <- 0 #if any probs are neg, change to 0 
    r1<-sample(30, size = 1, replace = TRUE, prob = new_crop_props)
    simulation_matrix[field,simulation] <-colnames(out)[r1+1]
    indi_field_area<-field_areas[field_areas$ID %in% out[,1],]
    area_by_field[field, which(names(area_by_field) == simulation_matrix[field,simulation])] <- indi_field_area[,1]
    crop_props[3,]<-colSums(area_by_field)
  }
  print(paste("finished", simulation-1,"out of 1000 simulations"))

}
end_time <- Sys.time()
time_elapsed <- end_time - start_time
print(paste("time for big loop:", time_elapsed))

#add in verification after running

print("finished!")
save.image(file='myEnvironment_prob_crop.RData') 
