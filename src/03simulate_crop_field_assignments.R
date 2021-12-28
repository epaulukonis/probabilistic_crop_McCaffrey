##Step 3: simulate field level crop classifications
print("do simulation for all counties")
simulate_start_time <- Sys.time()
print("stepping into 03simulate_crop_field_assignments.R")
print(Sys.time())
nsims <- 1000
print(dim(probs_by_fields))
#note that you will specify which county in 02
print('check 02 for county')


# create empty simulation matrix
simulation_matrix<-as.data.frame(matrix(data=NA,nrow=nrow(probs_by_fields),ncol=nsims)) #set up empty df to hold simulations
print(dim(simulation_matrix))
colnames(simulation_matrix)[1:nsims]<-paste0("Sim",1:nsims,"")
ID<-unique(probs_by_fields$ID)
print(dim(ID))
print(head(ID))
simulation_matrix<-cbind(ID,simulation_matrix)
print('dim sim mat')
print(dim(simulation_matrix))
print('dim of field areas')
print(dim(field_areas))
print('dim of probs by fields')
print(dim(probs_by_fields))

#create field areas and total crop area dataframes
print('assign field areas based on id')
field_areas<-field_areas[(field_areas$ID %in% probs_by_fields$ID),]
print('dim of new field areas')
print(dim(field_areas))
print(dim(probs_by_fields))
print(names(field_areas))

total_crop_and_field_area<-probs_by_fields[,2:31]*field_areas$field_areas
print(dim(total_crop_and_field_area))
total_crop_and_field_area<-as.data.frame(colSums(total_crop_and_field_area))
colnames(total_crop_and_field_area)[1]<-'crop_total'
total_crop_and_field_area$field_tot<-0
total_crop_and_field_area<-as.data.frame(t(total_crop_and_field_area))
colnames(total_crop_and_field_area)<-colnames(probs_by_fields)[2:31]
print(head(total_crop_and_field_area))


#dataframe for updating the areas that matches ncol and nrow of simulation_matrix
area_by_field<-probs_by_fields[,2:31]
area_by_field[,1:30]<-0
area_by_field<-as.data.frame(area_by_field)

##loaded environment contains everything above up to here
crop_probs<-as.data.frame(matrix(data=0,nrow=4,ncol=30)) #empty data-frame for calculating new probabilities
print(total_crop_and_field_area[1,])
crop_probs[2,]<-total_crop_and_field_area[1,]
names(crop_probs)<-names(probs_by_fields[,2:31])
row.names(crop_probs)<-c("orig_prob","total_crop","crop_so_far","updated_prob")
print(crop_probs)

#field loop
print("big loop over simulations and fields")
start_time <- Sys.time()
for (simulation in 1:nsims+1){ #1000
  sim_start_time <- Sys.time()
  for (field in 1:nrow(simulation_matrix)){ #16000
    out<-probs_by_fields[probs_by_fields$ID %in% simulation_matrix[field,1],]
    #print(out)
    crop_probs[1,]<-out[,2:31] #pull out crop probs
    crop_probs[4,]<-ifelse(crop_probs[3,] >=  crop_probs[2,] & crop_probs[1,] !=0, 0.00001, (1-(crop_probs[3,]/crop_probs[2,])) * crop_probs[1,]) #if total crop area = sum of field area, automatically assign 0 prob
    new_crop_probs<- crop_probs[4,]
    #new_crop_probs<-(1-(crop_probs[3,]/crop_probs[2,]))*crop_probs[1,]
    new_crop_probs<-rapply(new_crop_probs, function(x) ifelse(is.nan(x),0,x), how="replace" )
    new_crop_probs[new_crop_probs < 0] <- 0 #if any probs are neg, change to 0; fixes issues where crop_probs[1,] == 0
   # print(new_crop_probs)
   # write.csv(new_crop_probs, file = file.path(root_data_out, "new_crop_probs_san.csv"))
   # print(crop_probs)
   # print(field)
    r1<-sample(30, size = 1, replace = TRUE, prob = new_crop_probs)
    simulation_matrix[field,simulation] <-colnames(out)[r1+1]
    indi_field_area<-field_areas[field_areas$ID %in% out[,1],]
    area_by_field[field, which(names(area_by_field) == simulation_matrix[field,simulation])] <- indi_field_area[,1]
    crop_probs[3,]<-colSums(area_by_field)
  }
  print(paste("finished", simulation-1,"out of 1000 simulations"))
  area_by_field[,1:30]<-0 #remake empty area dataframe each sim
  area_by_field<-as.data.frame(area_by_field)

}

simulate_end_time <- Sys.time()
simulate_time_elapsed <- simulate_end_time - simulate_start_time
print(paste("time for big loop:", simulate_time_elapsed))

print("saving simulation_matrix object; specify county at end of each name")
print(Sys.time())
saveRDS(simulation_matrix, file = file.path(root_data_out, "simulation_matrix_sac"))
write.csv(simulation_matrix, file = file.path(root_data_out, "simulation_matrix_sac.csv"))

print("saving crop_probs object")
print(Sys.time())
saveRDS(crop_probs, file = file.path(root_data_out, "crop_probs_sac"))
write.csv(crop_probs, file = file.path(root_data_out, "crop_probs_sac.csv"))

print("saving independent field areas by sim object")
print(Sys.time())
saveRDS(total_crop_and_field_area, file = file.path(root_data_out, "total_crop_and_field_area_sac"))
write.csv(total_crop_and_field_area, file = file.path(root_data_out, "total_crop_and_field_area_sac.csv"))

print("saving area_by_field object")
print(Sys.time())
saveRDS(field_areas, file = file.path(root_data_out, "field_areas_sac"))
write.csv(field_areas, file = file.path(root_data_out, "field_areas_sac.csv"))


print("done with 03_simulate_crop_field_assignments")
print(Sys.time())