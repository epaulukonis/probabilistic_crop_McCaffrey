
simulate_start_time <- Sys.time()
print("stepping into 04verify_data_and_create_figures.R")
print(Sys.time())

##create boxplot of each crop from the sims showing distribution of total crop areas and then the line with the original areas
#so we need the area total by crop for each sim

#field_areas is area_f
#sum_mat is probs by fields

mat_n_f<-merge(mat_n,area_f, by='ID')
mat_n_f<-mat_n_f[,c(1,4,2:3)]

test<-setNames(lapply(names(mat_n_f)[-2], function(x) cbind(mat_n_f[2], mat_n_f[x])), names(mat_n_f)[-2])
test[1]<-NULL
IDs<-mat_n_f$ID
test<-lapply(test, cbind, IDs)
colnames<-c("area_field",'Crop','ID')
test<-lapply(test, setNames, colnames)

crop_area_calc<-function(x){
x %>% 
  group_by(Crop)%>% 
  summarise(Area_Crop = sum(area_field))}
test_f<-lapply(test, crop_area_calc)

list_of_output<-list()  
for (sim in 1:length(test_f)){
  output <- test_f[[sim]]
  output<-merge(crops_all, output, by="Crop", all.x=TRUE)
  output[is.na(output)] <- 0
  list_of_output[[sim]]<-output
}
fincalc<-Map(cbind, list_of_output, ID = (1:length( list_of_output)))

fin<-do.call("rbind",fincalc)
fin$Area_Crop<-as.integer(fin$Area_Crop)

orig_area<-as.data.frame(t(area_c[1,]))
orig_area$Crop<-row.names(orig_area)
row.names(orig_area)<-NULL
orig_area<-orig_area[order(orig_area$Crop),]

sum(orig_area$crop_total)
fin %>%
  group_by(ID) %>% 
  transmute(Total=sum(Area_Crop))

sum(var[2,1:30])
sum(var[3,1:30])

#final

simulation_matrix_f<-merge(simulation_matrix,field_areas, by='ID')
simulation_matrix_f<-simulation_matrix_f[,c(1,4,2:3)]

list_of_sims<-setNames(lapply(names(simulation_matrix_f)[-2], function(x) cbind(simulation_matrix_f[2], simulation_matrix_f[x])), names(simulation_matrix_f)[-2])
list_of_sims[1]<-NULL
IDs<-simulation_matrix_f$ID
list_of_sims<-lapply(list_of_sims, cbind, IDs)
colnames<-c("area_field",'sim','ID')
list_of_sims<-lapply(list_of_sims, setNames, colnames)

crop_area_calc<-function(x){
  x %>% 
    group_by(sim)%>% 
    summarise(Area_Crop = sum(area_field))}
list_of_sims_areas<-lapply(list_of_sims, crop_area_calc)

crops_all<-as.data.frame(matrix(data=names(probs_by_fields[2:31]),nrow=30,ncol=1))
colnames(crops_all)[1]<-'Crop'

list_of_output<-list()  
for (sim in 1:length(list_of_sims_areas)){
  output <- list_of_sims_areas[[sim]]
  output<-merge(crops_all, output, by="Crop", all.x=TRUE)
  output[is.na(output)] <- 0
  list_of_output[[sim]]<-output
}
compiled_areas_by_crop_sim<-Map(cbind, list_of_output, unique.id = (1:length( list_of_output)))

compiled_areas_fin<-do.call("rbind",compiled_areas_by_crop_sim)
compiled_areas_fin$Area_Crop<-as.integer(compiled_areas_fin$Area_Crop)



#figure showing reduction of autocorrelation?


