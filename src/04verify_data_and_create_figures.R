simulate_start_time <- Sys.time()
print("stepping into 04verify_data_and_create_figures.R")
print(Sys.time())


#Figure 1, Workflow
window<-extent(-2200000, -2160000, 1980000, 2000000)
r1<-crop(crop_raster_stack[[1]], window)
plot(r1)
plot(counties_trans[[3]], add=T)
counties_ca <- file.path(root_data_in, "ca_counties")
counties_shapes <- readOGR(dsn =  counties_ca, layer = "CA_Counties_TIGER2016")
sac.sub <- counties_shapes[counties_shapes$NAME == 'Sacramento',] 
sac.sub<-spTransform(sac.sub,crs(r1))
plot(r1)
plot(sac.sub, add=T)
counties_trans_sac<-counties_trans[[3]]
counties_trans_sac$crop<-simulation_matrix[,3]
plot(counties_trans[[3]], add=T)

vernal <- readOGR(dsn =  root_data_out, layer = "VPs2012remap")
vern.sub<-spTransform(vernal,crs(r1))
plot(vern.sub, add=T, col='blue')
plot(counties_trans[[3]], add=T)




##Figure 2, VP area

vernal <- readOGR(dsn =  root_data_out, layer = "VPs2012remap")
plot(vernal)
vern.sub<-spTransform(vernal,crs(crop_raster_stack[[1]]))



r1<-file.path(root_data_out, list.files(path=root_data_out, pattern='.tif$', all.files=T,full.names=F))
bf<-raster(r1[1])
plot(bf)

window<-extent(-121.5, -121.1, 38,38.3)
r1<-crop(bf, window)
plot(r1)

##Figure 3, Boxplot
#Madera----
sim_mat<-file.path(root_data_out, "simulation_matrix_mad.csv")
simulation_matrix<-read.csv(sim_mat)[,-1]
field_areas<-file.path(root_data_out, "field_areas_mad.csv")
field_areas<-read.csv(field_areas)[,-1]
simulation_matrix_f<-merge(simulation_matrix,field_areas, by='ID')
simulation_matrix_f<-simulation_matrix_f[,c(1,1002,2:1001)]
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

total_crop_and_field_area<-file.path(root_data_out, "total_crop_and_field_area_mad.csv")
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
fin_total_sum_areas[2,]
sum(as.numeric(orig_area$Area_Crop)) - fin_total_sum_areas[2,]

#let's add in the ratio of each crop for each sim to its original area
namesc<-c("Alfalfa", "Almond", "Cabbage", "Cantaloupes", "Corn", "Cotton", "Cucumbers", "Dry Beans", "Eggplants", "Fallow",
          "Grapes","Honeydew Melons","Lettuce","Misc.","Non-Crop","Oats","Oranges","Pears","Pecans","Peppers","Pistachios","Pomegranates",
          "Potatoes","Pumpkins","Soybeans","Squash","Sweet Potatoes","Tomatoes","Walnuts","Watermelons")
orig_area$Crops<-namesc
compiled_areas_fin<-merge(compiled_areas_fin,orig_area, by = 'Crop')
compiled_areas_fin<-compiled_areas_fin[order(compiled_areas_fin$ID.x), ]
compiled_areas_fin$Ratio<-as.numeric(compiled_areas_fin$Area_Crop.x)/as.numeric(compiled_areas_fin$Area_Crop.y)


#for now, let's remove the rows with NAN, because those simulations won't have those crops represented
#we can hope/assume out of 1000, all crops should be somewhat represented

compiled_areas_fin<-na.omit(compiled_areas_fin)
colnames(compiled_areas_fin)[6]<-'Crops'
label_area<-orig_area[orig_area$Crops %in% compiled_areas_fin$Crops,]
label_area$Area_Crop<-round(as.numeric(label_area$Area_Crop)*0.00024711,1)
label_area = compiled_areas_fin %>%
  group_by(Crops) %>%
  summarize(ypos = median(Ratio) + 1.10)%>%
  inner_join(., label_area)

#compiled_areas_fin<-compiled_areas_fin[compiled_areas_fin$Crops %in% orig_area$Crops,]
orig_area$Area_Crop<-round(as.numeric(orig_area$Area_Crop)*0.00024711,1)
write.csv(orig_area, file = file.path(root_data_out, "madera_orig.csv"))

compiled_areas_fin %>% ggplot(aes(x=Crops, y=Ratio, fill=Crops)) + 
  geom_boxplot()+
  scale_y_continuous(breaks=c(0,1,2,3))+
  coord_cartesian(ylim = c(0, 3))+
  # geom_text(data = label_area, aes(label = Area_Crop, y = ypos, color=Crops),
  #            position = position_dodge(width = .50),
  #           angle=45, show.legend = FALSE, fontface='bold')+
  # geom_label(data = label_area,
  #            aes(y = ypos, label = Area_Crop), vjust=0.5, fontface='bold', show.legend = F)+
  xlab("Crop - Madera County") + 
  ylab ("Ratio of Original Area to Simulated Area by Crop") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9))

#what percent of time is that being assigned zero? 
  #certain crops should not be assigned total crop zero
  #well-behaved simulation? accept them only if well-behaved
  #only accept if they're not zero for large crops, or other issues


#Merced----
sim_mat<-file.path(root_data_out, "simulation_matrix_mer.csv")
simulation_matrix<-read.csv(sim_mat)[,-1]
field_areas<-file.path(root_data_out, "field_areas_mer.csv")
field_areas<-read.csv(field_areas)[,-1]
simulation_matrix_f<-merge(simulation_matrix,field_areas, by='ID')
simulation_matrix_f<-simulation_matrix_f[,c(1,1002,2:1001)]
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

total_crop_and_field_area<-file.path(root_data_out, "total_crop_and_field_area_mer.csv")
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
fin_total_sum_areas[2,]
sum(as.numeric(orig_area$Area_Crop)) - fin_total_sum_areas[2,]

#let's add in the ratio of each crop for each sim to its original area
namesc<-c("Alfalfa", "Almond", "Cabbage", "Cantaloupes", "Corn", "Cotton", "Cucumbers", "Dry Beans", "Eggplants", "Fallow",
          "Grapes","Honeydew Melons","Lettuce","Misc.","Non-Crop","Oats","Oranges","Pears","Pecans","Peppers","Pistachios","Pomegranates",
          "Potatoes","Pumpkins","Soybeans","Squash","Sweet Potatoes","Tomatoes","Walnuts","Watermelons")
orig_area$Crops<-namesc
compiled_areas_fin<-merge(compiled_areas_fin,orig_area, by = 'Crop')
compiled_areas_fin<-compiled_areas_fin[order(compiled_areas_fin$ID.x), ]
compiled_areas_fin$Ratio<-as.numeric(compiled_areas_fin$Area_Crop.x)/as.numeric(compiled_areas_fin$Area_Crop.y)

#for now, let's remove the rows with NAN, because those simulations won't have those crops represented
#we can hope/assume out of 1000, all crops should be somewhat represented

compiled_areas_fin<-na.omit(compiled_areas_fin)
colnames(compiled_areas_fin)[6]<-'Crops'
label_area<-orig_area[orig_area$Crops %in% compiled_areas_fin$Crops,]
label_area$Area_Crop<-round(as.numeric(label_area$Area_Crop)*0.00024711,1)
label_area = compiled_areas_fin %>%
  group_by(Crops) %>%
  summarize(ypos = median(Ratio) + 1.10)%>%
  inner_join(., label_area)

#compiled_areas_fin<-compiled_areas_fin[compiled_areas_fin$Crops %in% orig_area$Crops,]

orig_area$Area_Crop<-round(as.numeric(orig_area$Area_Crop)*0.00024711,1)
write.csv(orig_area, file = file.path(root_data_out, "merced_orig.csv"))

compiled_areas_fin %>% ggplot(aes(x=Crops, y=Ratio, fill=Crops)) + 
  geom_boxplot()+
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6))+
  coord_cartesian(ylim = c(0, 6))+
  # geom_text(data = label_area, aes(label = Area_Crop, y = ypos, color=Crops),
  #            position = position_dodge(width = .50),
  #           angle=45, show.legend = FALSE, fontface='bold')+
  # geom_label(data = label_area,
  #            aes(y = ypos, label = Area_Crop), vjust=0.5, fontface='bold', show.legend = F)+
  xlab("Crop - Merced County") + 
  ylab ("Ratio of Original Area to Simulated Area by Crop") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9))


#Sacramento----
sim_mat<-file.path(root_data_out, "simulation_matrix_sac.csv")
simulation_matrix<-read.csv(sim_mat)[,-1]
field_areas<-file.path(root_data_out, "field_areas_sac.csv")
field_areas<-read.csv(field_areas)[,-1]
simulation_matrix_f<-merge(simulation_matrix,field_areas, by='ID')
simulation_matrix_f<-simulation_matrix_f[,c(1,1002,2:1001)]
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
fin_total_sum_areas[2,]
sum(as.numeric(orig_area$Area_Crop)) - fin_total_sum_areas[2,]

#let's add in the ratio of each crop for each sim to its original area
namesc<-c("Alfalfa", "Almond", "Cabbage", "Cantaloupes", "Corn", "Cotton", "Cucumbers", "Dry Beans", "Eggplants", "Fallow",
          "Grapes","Honeydew Melons","Lettuce","Misc.","Non-Crop","Oats","Oranges","Pears","Pecans","Peppers","Pistachios","Pomegranates",
          "Potatoes","Pumpkins","Soybeans","Squash","Sweet Potatoes","Tomatoes","Walnuts","Watermelons")
orig_area$Crops<-namesc
compiled_areas_fin<-merge(compiled_areas_fin,orig_area, by = 'Crop')
compiled_areas_fin<-compiled_areas_fin[order(compiled_areas_fin$ID.x), ]
compiled_areas_fin$Ratio<-as.numeric(compiled_areas_fin$Area_Crop.x)/as.numeric(compiled_areas_fin$Area_Crop.y)

#for now, let's remove the rows with NAN, because those simulations won't have those crops represented
#we can hope/assume out of 1000, all crops should be somewhat represented

compiled_areas_fin<-na.omit(compiled_areas_fin)
colnames(compiled_areas_fin)[6]<-'Crops'
label_area<-orig_area[orig_area$Crops %in% compiled_areas_fin$Crops,]
label_area$Area_Crop<-round(as.numeric(label_area$Area_Crop)*0.00024711,1)
label_area = compiled_areas_fin %>%
  group_by(Crops) %>%
  summarize(ypos = median(Ratio) + 1.10)%>%
  inner_join(., label_area)

#compiled_areas_fin<-compiled_areas_fin[compiled_areas_fin$Crops %in% orig_area$Crops,]
orig_area$Area_Crop<-round(as.numeric(orig_area$Area_Crop)*0.00024711,1)
write.csv(orig_area, file = file.path(root_data_out, "sacramento_orig.csv"))

compiled_areas_fin %>% ggplot(aes(x=Crops, y=Ratio, fill=Crops)) + 
  geom_boxplot()+
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9))+
  coord_cartesian(ylim = c(0, 9))+
  # geom_text(data = label_area, aes(label = Area_Crop, y = ypos, color=Crops),
  #            position = position_dodge(width = .50),
  #           angle=45, show.legend = FALSE, fontface='bold')+
  # geom_label(data = label_area,
  #            aes(y = ypos, label = Area_Crop), vjust=0.5, fontface='bold', show.legend = F)+
  xlab("Crop - Sacramento County") + 
  ylab ("Ratio of Original Area to Simulated Area by Crop") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9))

#San Joaquin----
sim_mat<-file.path(root_data_out, "simulation_matrix_stan.csv")
simulation_matrix<-read.csv(sim_mat)[,-1]
field_areas<-file.path(root_data_out, "field_areas_stan.csv")
field_areas<-read.csv(field_areas)[,-1]
simulation_matrix_f<-merge(simulation_matrix,field_areas, by='ID')
simulation_matrix_f<-simulation_matrix_f[,c(1,1002,2:1001)]
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

total_crop_and_field_area<-file.path(root_data_out, "total_crop_and_field_area_stan.csv")
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
fin_total_sum_areas[2,]
sum(as.numeric(orig_area$Area_Crop)) - fin_total_sum_areas[2,]

#let's add in the ratio of each crop for each sim to its original area
namesc<-c("Alfalfa", "Almond", "Cabbage", "Cantaloupes", "Corn", "Cotton", "Cucumbers", "Dry Beans", "Eggplants", "Fallow",
          "Grapes","Honeydew Melons","Lettuce","Misc.","Non-Crop","Oats","Oranges","Pears","Pecans","Peppers","Pistachios","Pomegranates",
          "Potatoes","Pumpkins","Soybeans","Squash","Sweet Potatoes","Tomatoes","Walnuts","Watermelons")
orig_area$Crops<-namesc
compiled_areas_fin<-merge(compiled_areas_fin,orig_area, by = 'Crop')
compiled_areas_fin<-compiled_areas_fin[order(compiled_areas_fin$ID.x), ]
compiled_areas_fin$Ratio<-as.numeric(compiled_areas_fin$Area_Crop.x)/as.numeric(compiled_areas_fin$Area_Crop.y)

#for now, let's remove the rows with NAN, because those simulations won't have those crops represented
#we can hope/assume out of 1000, all crops should be somewhat represented

compiled_areas_fin<-na.omit(compiled_areas_fin)
colnames(compiled_areas_fin)[6]<-'Crops'
label_area<-orig_area[orig_area$Crops %in% compiled_areas_fin$Crops,]
label_area$Area_Crop<-round(as.numeric(label_area$Area_Crop)*0.00024711,1)
label_area = compiled_areas_fin %>%
  group_by(Crops) %>%
  summarize(ypos = median(Ratio) + 1.10)%>%
  inner_join(., label_area)

#compiled_areas_fin<-compiled_areas_fin[compiled_areas_fin$Crops %in% orig_area$Crops,]
orig_area$Area_Crop<-round(as.numeric(orig_area$Area_Crop)*0.00024711,1)
write.csv(orig_area, file = file.path(root_data_out, "sanjoaquin_orig.csv"))

compiled_areas_fin %>% ggplot(aes(x=Crops, y=Ratio, fill=Crops)) + 
  geom_boxplot()+
  scale_y_continuous(breaks=c(0,1,2,3,4,5,7,8))+
  coord_cartesian(ylim = c(0, 8))+
  # geom_text(data = label_area, aes(label = Area_Crop, y = ypos, color=Crops),
  #            position = position_dodge(width = .50),
  #           angle=45, show.legend = FALSE, fontface='bold')+
  # geom_label(data = label_area,
  #            aes(y = ypos, label = Area_Crop), vjust=0.5, fontface='bold', show.legend = F)+
  xlab("Crop - San Joaquin County") + 
  ylab ("Ratio of Original Area to Simulated Area by Crop") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9))

#Stanislaus ----
sim_mat<-file.path(root_data_out, "simulation_matrix_san.csv")
simulation_matrix<-read.csv(sim_mat)[,-1]
field_areas<-file.path(root_data_out, "field_areas_san.csv")
field_areas<-read.csv(field_areas)[,-1]
simulation_matrix_f<-merge(simulation_matrix,field_areas, by='ID')
simulation_matrix_f<-simulation_matrix_f[,c(1,1002,2:1001)]
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

total_crop_and_field_area<-file.path(root_data_out, "total_crop_and_field_area_san.csv")
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
fin_total_sum_areas[2,]
sum(as.numeric(orig_area$Area_Crop)) - fin_total_sum_areas[2,]

#let's add in the ratio of each crop for each sim to its original area
namesc<-c("Alfalfa", "Almond", "Cabbage", "Cantaloupes", "Corn", "Cotton", "Cucumbers", "Dry Beans", "Eggplants", "Fallow",
          "Grapes","Honeydew Melons","Lettuce","Misc.","Non-Crop","Oats","Oranges","Pears","Pecans","Peppers","Pistachios","Pomegranates",
          "Potatoes","Pumpkins","Soybeans","Squash","Sweet Potatoes","Tomatoes","Walnuts","Watermelons")
orig_area$Crops<-namesc
compiled_areas_fin<-merge(compiled_areas_fin,orig_area, by = 'Crop')
compiled_areas_fin<-compiled_areas_fin[order(compiled_areas_fin$ID.x), ]
compiled_areas_fin$Ratio<-as.numeric(compiled_areas_fin$Area_Crop.x)/as.numeric(compiled_areas_fin$Area_Crop.y)

#for now, let's remove the rows with NAN, because those simulations won't have those crops represented
#we can hope/assume out of 1000, all crops should be somewhat represented

compiled_areas_fin<-na.omit(compiled_areas_fin)
colnames(compiled_areas_fin)[6]<-'Crops'
label_area<-orig_area[orig_area$Crops %in% compiled_areas_fin$Crops,]
label_area$Area_Crop<-round(as.numeric(label_area$Area_Crop)*0.00024711,1)
label_area = compiled_areas_fin %>%
  group_by(Crops) %>%
  summarize(ypos = median(Ratio) + 1.10)%>%
  inner_join(., label_area)

#compiled_areas_fin<-compiled_areas_fin[compiled_areas_fin$Crops %in% orig_area$Crops,]
orig_area$Area_Crop<-round(as.numeric(orig_area$Area_Crop)*0.00024711,1)
write.csv(orig_area, file = file.path(root_data_out, "stanislaus_orig.csv"))

compiled_areas_fin %>% ggplot(aes(x=Crops, y=Ratio, fill=Crops)) + 
  geom_boxplot()+
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  coord_cartesian(ylim = c(0, 12))+
  # geom_text(data = label_area, aes(label = Area_Crop, y = ypos, color=Crops),
  #            position = position_dodge(width = .50),
  #           angle=45, show.legend = FALSE, fontface='bold')+
  # geom_label(data = label_area,
  #            aes(y = ypos, label = Area_Crop), vjust=0.5, fontface='bold', show.legend = F)+
  xlab("Crop - Stanislaus County") + 
  ylab ("Ratio of Original Area to Simulated Area by Crop") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9))


