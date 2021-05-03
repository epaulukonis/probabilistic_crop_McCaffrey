
simulate_start_time <- Sys.time()
print("stepping into 04verify_data_and_create_figures.R")
print(Sys.time())

##create boxplot of each crop from the sims showing distribution of total crop areas and then the line with the original areas
#so we need the area total by crop for each sim

#field_areas is area_f

mat_n_f<-merge(mat_n,area_f, by='ID')

mat__f %>% 
  group_by(unique(Sim1))%>% 
  summarise(Frequency = sum(area_field))

area_by_field[field, which(names(area_by_field) == simulation_matrix[field,simulation])] <- indi_field_area[,1]
crop_props[3,]<-colSums(area_by_field)



#figure showing reduction of autocorrelation?


