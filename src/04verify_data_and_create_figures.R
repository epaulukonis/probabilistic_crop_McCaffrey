
simulate_start_time <- Sys.time()
print("stepping into 04verify_data_and_create_figures.R")
print(Sys.time())

##create boxplot of each crop from the sims showing distribution of total crop areas and then the line with the original areas
#so we need the area total by crop for each sim

#field_areas is area_f

mat_n_f<-merge(mat_n,area_f, by='ID')
mat_n_f<-mat_n_f[,c(1,4,2:3)]

cols<-names(mat_n_f[,3:4])

out<-mat_n_f %>% 
  group_by(Sim1)%>% 
  summarise(Area_Crop = sum(area_field))



#fix tomorrow


#figure showing reduction of autocorrelation?


