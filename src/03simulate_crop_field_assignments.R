##Step 3: simulate field level crop classifications
simulate_start_time <- Sys.time()
print("stepping into 03simulate_crop_field_assignments.R")
print(Sys.time())
nsims <- 1000

#note that you will specify which county in 02
print('county information loaded from 02')

# create empty simulation matrix
simulation_matrix<-as.data.frame(matrix(data=NA,nrow=nrow(probs_by_fields),ncol=nsims)) #set up empty df to hold simulations
colnames(simulation_matrix)[1:nsims]<-paste0("Sim",1:nsims,"")
ID<-unique(probs_by_fields$ID)
simulation_matrix<-cbind(ID,simulation_matrix)

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
for (simulation in 2:nsims+1){ #1000  
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

#add in verification after running
simulate_end_time <- Sys.time()
simulate_time_elapsed <- simulate_end_time - simulate_start_time
print(paste("time for big loop:", simulate_time_elapsed))

print("saving simulation_matrix object")
print(Sys.time())
saveRDS(simulation_matrix, file = file.path(root_data_out, "simulation_matrix"))


print("saving crop_probs object")
print(Sys.time())
saveRDS(crop_probs, file = file.path(root_data_out, "crop_probs"))

print("saving independent field areas by sim object")
print(Sys.time())
saveRDS(indi_field_area, file = file.path(root_data_out, "indi_field_area"))

print("saving area_by_field object")
print(Sys.time())
saveRDS(area_by_field, file = file.path(root_data_out, "area_by_field"))

