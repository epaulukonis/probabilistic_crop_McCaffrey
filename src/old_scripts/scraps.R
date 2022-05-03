# simulation_matrix_mer_filename <- file.path(root_data_out,"simulation_matrix_mer")
# crop_probs_filename <- file.path(root_data_out,"crop_probs")
# total_crop_and_field_area_filename <- file.path(root_data_out,"total_crop_and_field_area")
# field_areas_filename <- file.path(root_data_out,"field_areas")
# if( file.exists(simulation_matrix_mer_filename)&&
#     file.exists(crop_probs_filename)&&
#     file.exists(total_crop_and_field_area_filename)&&
#     file.exists(field_areas_filename))
# {
# files exist 
#print("files do exist, no need to redo the merced process; move on to other counties")


# print("calculating raster areas")
# #calculate the area (m2) of each type of raster (crop/non-crop) for each crop type
# m<-list()
# for (i in 1:29) {
#   y <- out[[i]]
#   m[[i]]<-as.matrix(tapply(area(y), y[], sum))
# }
#
# m_a<-lapply(m, `length<-`, max(lengths(m))) #change length of list to all match
# total_crop_and_field_area<- data.frame(matrix(unlist(m_a), nrow=length(m_a), byrow=TRUE)) #turn into workable dataframe for total crop area
# NC<-total_crop_and_field_area[1,1]
# total_crop_and_field_area<-as.data.frame(total_crop_and_field_area[,2])
# total_crop_and_field_area[is.na(total_crop_and_field_area)] <- 0
# total_crop_and_field_area[30,]<-NC-sum(total_crop_and_field_area[1:29,])
# colnames(total_crop_and_field_area)[1]<-'crop_total'
# total_crop_and_field_area$field_tot<-0
# total_crop_and_field_area<-as.data.frame(t(total_crop_and_field_area))
# colnames(total_crop_and_field_area)<-colnames(probs_by_fields)[2:31]

#   field_areas<-field_areas[(field_areas$ID %in% simulation_matrix$ID),]
#   total_crop_and_field_area<-probs_by_fields[,2:31]*field_areas$area_field
#   total_crop_and_field_area<-as.data.frame(colSums(total_crop_and_field_area))
#   colnames(total_crop_and_field_area)[1]<-'crop_total'
#   total_crop_and_field_area$field_tot<-0
#   total_crop_and_field_area<-as.data.frame(t(total_crop_and_field_area))
#   colnames(total_crop_and_field_area)<-colnames(simulation_matrix)[2:31]
# 
# 
#   #dataset for updating the areas that matches ncol and nrow of simulation_matrix
#   area_by_field<-probs_by_fields[,2:31]
#   area_by_field[,1:30]<-0
#   area_by_field<-as.data.frame(area_by_field)
# 
#   ##loaded environment contains everything above up to here
#   crop_probs<-as.data.frame(matrix(data=0,nrow=4,ncol=30)) #empty data-frame for calculating new probabilities
#   crop_probs[2,]<-total_crop_and_field_area[1,]
#   names(crop_probs)<-names(probs_by_fields[,2:31])
#   row.names(crop_probs)<-c("orig_prob","total_crop","crop_so_far","updated_prob")
# 
#   #field loop
#   print("big loop over simulations and fields")
#   start_time <- Sys.time()
#   for (simulation in 1:nsims+1){ #1000
#     sim_start_time <- Sys.time()
#     for (field in 1:nrow(simulation_matrix)){ #16000
#       out<-probs_by_fields[probs_by_fields$ID %in% simulation_matrix[field,1],]
#       crop_probs[1,]<-out[,2:31] #pull out crop probs
#       crop_probs[4,]<-ifelse(crop_probs[3,] >=  crop_probs[2,] & crop_probs[1,] !=0, 0.00001, (1-(crop_probs[3,]/ crop_probs[2,]))* crop_probs[1,]) #if total crop area = sum of field area, automatically assign 0 prob
#       new_crop_probs<- crop_probs[4,]
#       #new_crop_probs<-(1-(crop_probs[3,]/crop_probs[2,]))*crop_probs[1,]
#       new_crop_probs<-rapply(new_crop_probs, function(x) ifelse(is.nan(x),0,x), how="replace" )
#       #new_crop_probs[new_crop_probs < 0] <- 0 #if any probs are neg, change to 0
#       r1<-sample(30, size = 1, replace = TRUE, prob = new_crop_probs)
#       simulation_matrix[field,simulation] <-colnames(out)[r1+1]
#       indi_field_area<-field_areas[field_areas$ID %in% out[,1],]
#       area_by_field[field, which(names(area_by_field) == simulation_matrix[field,simulation])] <- indi_field_area[,1]
#       crop_probs[3,]<-colSums(area_by_field)
#     }
#     print(paste("finished", simulation,"out of 1000 simulations"))
#     area_by_field[,1:30]<-0 #remake empty area dataframe each sim
#     area_by_field<-as.data.frame(area_by_field)
#   }
#   simulate_end_time <- Sys.time()
#   simulate_time_elapsed <- simulate_end_time - simulate_start_time
#   print(paste("time for big loop:", simulate_time_elapsed))
# 
#   print("saving simulation_matrix object; specify county")
#   print(Sys.time())
#  # saveRDS(simulation_matrix, file = file.path(root_data_out, "simulation_matrix_stan"))
#   simulation_matrix_filename <- file.path(root_data_out, "simulation_matrix_mer")
#   simulation_matrix<-readRDS(file = simulation_matrix_filename) 
#   write.csv(simulation_matrix, file = file.path(root_data_out, "simulation_matrix_mer.csv"))
#   
#   print("saving crop_probs object")
#   print(Sys.time())
#  # saveRDS(crop_probs, file = file.path(root_data_out, "crop_probs_stan"))
#   crop_probs_filename <- file.path(root_data_out, "crop_probs")
#   crop_probs<-readRDS(file = crop_probs_filename) 
#   write.csv(crop_probs, file = file.path(root_data_out, "crop_probs_mer.csv"))
#   
#   print("saving independent field areas by sim object")
#   print(Sys.time())
#   #saveRDS(total_crop_and_field_area, file = file.path(root_data_out, "total_crop_and_field_area_stan"))
#   total_crop_and_field_area_filename <- file.path(root_data_out, "total_crop_and_field_area")
#   total_crop_and_field_area<-readRDS(file = total_crop_and_field_area_filename) 
#   write.csv(total_crop_and_field_area, file = file.path(root_data_out, "total_crop_and_field_area_mer.csv"))
#   
#   print("saving area_by_field object")
#   print(Sys.time())
#   #saveRDS(field_areas, file = file.path(root_data_out, "field_areas_stan"))
#   field_areas_filename <- file.path(root_data_out, "field_areas")
#   field_areas<-readRDS(file = field_areas_filename) 
#   write.csv(field_areas, file = file.path(root_data_out, "field_areas_mer.csv"))
#   
# }else {


# print("calculating raster areas")
# #calculate the area (m2) of each type of raster (crop/non-crop) for each crop type
# m<-list()
# for (i in 1:29) {
#   y <- out[[i]]
#   m[[i]]<-as.matrix(tapply(area(y), y[], sum))
# }
# 
# m_a<-lapply(m, `length<-`, max(lengths(m))) #change length of list to all match
# total_crop_and_field_area<- data.frame(matrix(unlist(m_a), nrow=length(m_a), byrow=TRUE)) #turn into workable dataframe for total crop area
# NC<-total_crop_and_field_area[1,1]
# total_crop_and_field_area<-as.data.frame(total_crop_and_field_area[,2])
# total_crop_and_field_area[is.na(total_crop_and_field_area)] <- 0
# total_crop_and_field_area[30,]<-NC-sum(total_crop_and_field_area[1:29,])
# colnames(total_crop_and_field_area)[1]<-'crop_total'
# total_crop_and_field_area$field_tot<-0
# total_crop_and_field_area<-as.data.frame(t(total_crop_and_field_area))
# colnames(total_crop_and_field_area)<-colnames(probs_by_fields)[2:31]
# }



# 
#   function to convert the rasters to binary
#   fun_c <- function(x) {
#     x[x>0] <- 1
#     return(x)
#   }
#   out <- stack(calc(county_raster_stack, fun_c)) #put that in a raster stack, make sure to specify raster stack layer
#   save probs_by_fields and out
#   print("saving out object")
#   print(Sys.time())
#   saveRDS(out, file = file.path(root_data_out, "out.rds"))


# 
#   function to convert the rasters to binary
#   fun_c <- function(x) {
#     x[x>0] <- 1
#     return(x)
#   }
#   out <- stack(calc(county_raster_stack, fun_c)) #put that in a raster stack, make sure to specify raster stack layer
#   save probs_by_fields and out
#   print("saving out object")
#   print(Sys.time())
#   saveRDS(out, file = file.path(root_data_out, "out.rds"))



#plot(bfc,axes=FALSE, box=FALSE) #probability raster 
plot(sj, add=T, col='blue',axes=FALSE, box=FALSE) #cropped vernal pools
five_san_sub <- san[san$sim5%in% crops, ]
plot(five_san_sub, add =T)
five_sac_sub <- sac[sac$sim5%in% crops, ]
plot(five_sac_sub, add =T)

#plot(bfc,axes=FALSE, box=FALSE) #probability raster 
# par(bg = "azure2")
# plot(sj, col='lightblue') 
#cropped vernal pools





#do a KDE of each of the crops across each sim as well
p2 <- ggplot(data=diamonds, aes(x=price, group=cut, fill=cut)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()



#almond
#corn
#cotton
#pistachios
#tomatoes
#walnuts