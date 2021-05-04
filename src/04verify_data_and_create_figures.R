
simulate_start_time <- Sys.time()
print("stepping into 04verify_data_and_create_figures.R")
print(Sys.time())

##create boxplot of each crop from the sims showing distribution of total crop areas and then the line with the original areas
#so we need the area total by crop for each sim

#field_areas is area_f

mat_n_f<-merge(mat_n,area_f, by='ID')
mat_n_f<-mat_n_f[,c(1,4,2:3)]

test<-setNames(lapply(names(mat_n_f)[-2], function(x) cbind(mat_n_f[2], mat_n_f[x])), names(mat_n_f)[-2])
test[1]<-NULL
IDs<-mat_n_f$ID
test<-lapply(test, cbind, IDs)
colnames<-c("area_field",'sim','ID')
test<-lapply(test, setNames, colnames)

crop_area_calc<-function(x){
x %>% 
  group_by(sim)%>% 
  summarise(Area_Crop = sum(area_field))}
test_f<-lapply(test, crop_area_calc)



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





#fix tomorrow


#figure showing reduction of autocorrelation?


