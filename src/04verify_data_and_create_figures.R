
simulate_start_time <- Sys.time()
print("stepping into 04verify_data_and_create_figures.R")
print(Sys.time())

##figure for workflow - Fig. 1
#madera county, alfalfa
window<-extent(-2200000, -2160000, 1980000, 2000000)
r1<-crop(crop_raster_stack[[1]], window)
plot(r1)
plot(counties_trans[[3]])
counties_ca <- file.path(root_data_in, "ca_counties")

counties_shapes <- readOGR(dsn =  counties_ca, layer = "CA_Counties_TIGER2016")
sac.sub <- counties_shapes[counties_shapes$NAME == 'Sacramento',] 
sac.sub<-spTransform(madera.sub,crs(r1))
plot(r1)
plot(sac.sub, add=T)
counties_trans_sac<-counties_trans[[3]]
counties_trans_sac$crop<-simulation_matrix[,3]

plot(counties_trans[[3]], add=T, col=col)



##figure for boxplot - Fig. 2

#sac
sim_mat<-file.path(root_data_out, "simulation_matrix_sac.csv")
simulation_matrix<-read.csv(sim_mat)
field_areas<-file.path(root_data_out, "field_areas_sac.csv")
field_areas<-read.csv(field_areas)

simulation_matrix_f<-merge(simulation_matrix,field_areas, by='ID')
simulation_matrix_f<-simulation_matrix_f[,c(1,1003,2:1002)]

list_of_sims<-setNames(lapply(names(simulation_matrix_f)[-2], function(x) cbind(simulation_matrix_f[2], simulation_matrix_f[x])), names(simulation_matrix_f)[-2])
list_of_sims[1]<-NULL
IDs<-simulation_matrix_f$ID
list_of_sims<-lapply(list_of_sims, cbind, IDs)
colnames<-c("area_field",'Crop','ID')
list_of_sims<-lapply(list_of_sims, setNames, colnames)

crop_area_calc<-function(x){
  x %>% 
    group_by(Crop)%>% 
    summarise(Area_Crop = sum(area_field))}
list_of_sims_areas<-lapply(list_of_sims, crop_area_calc)

crop_names<-c(names(crop_raster_stack), 'NC')

crops_all<-as.data.frame(matrix(crop_names,nrow=30,ncol=1))
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
colnames(compiled_areas_fin)[3]<-'ID'

fin_total_sum_areas<-compiled_areas_fin %>%
  group_by(ID) %>% 
  summarize(Total=sum(Area_Crop)) #this gives sum of each total area with new crop assignments
#there's going to be a slight discrepancy between the total field area and the total area of the raster crops
#this is likely because of a small (<0.25 acre) difference between the cropped pixels and the fields (i.e., a few pixels not covering the extent of the fields)

total_crop_and_field_area<-file.path(root_data_out, "total_crop_and_field_area_sac.csv")
total_crop_and_field_area<-read.csv(total_crop_and_field_area)


orig_area<-as.data.frame(t(total_crop_and_field_area[1,]))
orig_area$Crop<-row.names(orig_area)
row.names(orig_area)<-NULL
orig_area<-orig_area[2:31,]
orig_area<-orig_area[order(orig_area$Crop),]
#orig_area$crop_total<-as.integer(orig_area$crop_total)
colnames(orig_area)[1]<-'Area_Crop'
orig_area<-orig_area[,c(2,1)]
orig_area$ID<-'Orig'

sum(as.numeric(orig_area$Area_Crop))

#let's add in the ratio of each crop for each sim to its original area

namesc<-c("Alfalfa", "Almond", "Cabbage", "Cantaloupes", "Corn", "Cotton", "Cucumbers", "Dry Beans", "Eggplants", "Fallow",
          "Grapes","Honeydew Melons","Lettuce","Misc.","Non-Crop","Oats","Oranges","Pears","Pecans","Peppers","Pistachios","Pomegranates",
          "Potatoes","Pumpkins","Soybeans","Squash","Sweet Potatoes","Tomatoes","Walnuts","Watermelons")
orig_area$Name<-namesc

compiled_areas_fin<-merge(compiled_areas_fin,orig_area, by = 'Crop')
compiled_areas_fin<-compiled_areas_fin[order(compiled_areas_fin$ID.x), ]
compiled_areas_fin$Ratio<-as.numeric(compiled_areas_fin$Area_Crop.x)/as.numeric(compiled_areas_fin$Area_Crop.y)

#for now, let's simple remove the rows with NAN, because those simulations won't have those crops represented
#we can hope/assume out of 1000, all crops should be somewhat represented

compiled_areas_fin<-na.omit(compiled_areas_fin)


compiled_areas_fin %>% ggplot(aes(x=Name, y=Ratio, fill=Name)) + 
  geom_boxplot()+
  coord_cartesian(ylim = c(-2, 6))+
  xlab("Crop - Sacramento County") + 
  ylab ("Ratio of Original Area to Simulated Area by Crop") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9))
dev.off()



