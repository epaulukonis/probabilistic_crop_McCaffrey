simulate_start_time <- Sys.time()
print("stepping into 04verify_data_and_create_figures.R")
print(Sys.time())


#Figure 3, Workflow----
window<-extent(-2165000, -2150000, 1925000, 1935000) 
plot(crop_raster_stack[[4]])
plot(window,add=T)
r1<-crop(crop_raster_stack[[8]], window)
plot(r1)
plot(counties_trans[[4]], add=T)
counties_trans_sac<-counties_trans[[4]]
counties_trans_sac$crop<-sim_mat_san$Sim10
counties_trans_sac<-gBuffer(counties_trans_sac, byid=T, width=0)
crop_san<-crop(counties_trans_sac,r1)
plot(r1, axes= F, box=F)
plot(crop_san, add=T, axes=F, box=F)
crop_san$ID<-1:nrow(crop_san)
crop_san$Crop<- factor(crop_san$crop, levels = c("NC","Almond_StudyArea","Corn_StudyArea","DryBeans_StudyArea",
                                                 "Grapes_StudyArea","Pumpkins_StudyArea","Tomatoes_StudyArea", "Walnuts_StudyArea" ))

fields_f <- fortify(crop_san, region = "ID")
colnames(fields_f)[6]<-'ID'
fields_fin <-merge(fields_f, crop_san@data,
                   by = "ID")

#FF9999
#FF9966
#CC6600
#336600
#FFFF33
#669933
#CCCCCC
w1<-ggplot() +
  geom_polygon(data = fields_fin, aes(x=long, y=lat, group=group, fill=Crop), colour='black') +
 scale_fill_manual(labels = c("Non-Crop", "Almond","Corn","Beans","Grapes","Pumpkins","Tomatoes","Walnuts"), values = c("#CCCCCC","#669933", "#336600","#99CC00","#FFCC33", "#CC6600","#FF9999","#FF9966")) + 
  coord_equal() +
  theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(title = "")
w1

scale_color_manual(labels = c("T999", "T888"), values = c("blue", "red")) +

# counties_ca <- file.path(root_data_in, "ca_counties")
# counties_shapes <- readOGR(dsn =  counties_ca, layer = "CA_Counties_TIGER2016")
# san.sub <- counties_shapes[counties_shapes$NAME == 'San Joaquin',] 
# san.sub<-spTransform(sac.sub,crs(r1))








##Figure 7, VP area----
vernal <- readOGR(dsn =  root_data_out, layer = "vp_vpfs_fCH_71FR7117.shp")
plot(vernal)
vern.sub<-spTransform(vernal,crs(crop_raster_stack[[1]]))
r1<-file.path(root_data_out, list.files(path=root_data_out, pattern='.tif$', all.files=T,full.names=F))
bf<-raster(r1[1]) #this raster contains the original crop probabilities; let's find an area of high prob near vernal pool
bf<-projectRaster(bf, crs = crs(crop_raster_stack[[1]])) #reproject
plot(bf)
window<-extent(-2155000, -2141500, 1938000, 1955000) #extent of vp with overlap of varied probability

bfc<-crop(bf, window)
plot(bfc)
plot(vern.sub, add=T, col='blue')

buff_1km<-gBuffer(vern.sub, byid = TRUE, width = 1000, capStyle="ROUND")
plot(buff_1km, add=T)

sj<-crop(vern.sub, window)
sj<-aggregate(sj, dissolve=T)
plot(sj)
sj

san<-gBuffer(counties_trans[[4]], byid=T, width=0)
#san<-crop(san, window)
san<-crop(san,buff_1km)

#this is an area between sacramento and san joaquin counties
plot(bfc,axes=FALSE, box=FALSE) #probability raster 
plot(san,add=T,axes=FALSE, box=FALSE) #cropped field levels
plot(sj, add=T, col='blue',axes=FALSE, box=FALSE) #cropped vernal pools
#plot(sac, add=T, axes=FALSE, box=FALSE) #cropped field levels

#time to add in the simulations for sac and san
#sim_mat_sac<-file.path(root_data_out, "simulation_matrix_sac.csv")
sim_mat_san<-file.path(root_data_out, "simulation_matrix_san.csv")
#sim_mat_sac<-read.csv(sim_mat_sac)[,-1]
sim_mat_san<-read.csv(sim_mat_san)[,-1]
#field_areas_sac<-file.path(root_data_out, "field_areas_sac.csv")
#field_areas_sac<-read.csv(field_areas_sac)[,-1]
field_areas_san<-file.path(root_data_out, "field_areas_san.csv")
field_areas_san<-read.csv(field_areas_san)[,-1]

#let's turn our cropped polygon field layers into a data-frame that we can then match to our sim matrices
#sac$ID<-1:nrow(sac)
san$ID<-1:nrow(san)
#sac.df <- as.data.frame(sac)
san.df <- as.data.frame(san)
#sac.df.f<-sac.df[sac.df$ID %in% sim_mat_sac$ID,] #remove any rows that may not be present in the final sim
#sac.sims<-sim_mat_sac[sim_mat_sac$ID %in% sac.df.f$ID,]
san.df.f<-san.df[san.df$ID %in% sim_mat_san$ID,] #remove any rows that may not be present in the final sim
san.sims<-sim_mat_san[sim_mat_san$ID %in% san.df.f$ID,]

hist_data<-as.data.frame(matrix(data=0,nrow=1000,ncol=2)) 
colnames(hist_data)[1]<-'BifenthrinCropArea'
colnames(hist_data)[2]<-'Sim'
hist_data[,2]<-colnames(san.sims)[2:1001]
for (sim in 2:ncol(san.sims)){
bif_crops_san<-san.sims[san.sims[,sim] %in% all_crops,1:2]
bif_crop_area_san<- field_areas_san[field_areas_san[,2] %in% bif_crops_san[,1],]
# bif_crops_sac<-sac.sims[sac.sims[,sim] %in% crops,1:sim]
# bif_crop_area_sac<- field_areas_sac[field_areas_sac[,2] %in% bif_crops_sac[,1],]
hist_data[sim-1,1] <-sum(bif_crop_area_san[,1])*0.00024711
}

quantile_sims<-as.data.frame(quantile(hist_data[,1], probs = c(0.05,0.5,0.95), names=F))
colnames(quantile_sims)[1]<-'area'
quantile_sims[,1]<-round(quantile_sims[,1], 2)
quantile_sims

 histy<-ggplot(hist_data, aes(x=BifenthrinCropArea)) + 
  geom_histogram(binwidth=75, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
   geom_vline(xintercept=3728.16 , color="black", linetype="dashed", size=1)+
   geom_vline(xintercept=4067.62, color="black", linetype="dashed", size=1)+
   geom_vline(xintercept=4409.97, color="black", linetype="dashed", size=1)+
   geom_vline(xintercept=5683.322, color="red", linetype="dashed", size=1)+
   ylab("Frequency")+
   xlab ("Crop Acres")+
   # geom_area(data = subset(hist_data, BifenthrinCropArea < 6419.570), fill = "grey") +
   # geom_area(data = subset(hist_data, BifenthrinCropArea > 8282.646 ), fill = "black") +
   theme_ipsum() +
   theme(
     text = element_text(size=14, face = 'bold'),
     axis.title.x=element_text(size = 14, face = 'bold'),
     axis.title.y=element_text(size = 14, face = 'bold'),
     plot.title = element_text(size=15)
   )
 histy

 theme(text = element_text(size=20),
       axis.text.x = element_text(angle=90, hjust=1)) 
 
#note; quantile does not return the specific row values, apparently. So I used a 'closest' function to pick my scenarios
 #this is not a permanent solution
 five_per<-hist_data[which.min(abs(3728.16-hist_data$BifenthrinCropArea)),]
 fifty_per<-hist_data[which.min(abs(4067.62-hist_data$BifenthrinCropArea)),]
 ninetyfive_per<-hist_data[which.min(abs(4409.97-hist_data$BifenthrinCropArea)),]
 
 perc<-rbind(five_per,fifty_per,ninetyfive_per)
 perc
 
 #sac$sim5<-sac.sims$Sim6
 san$sim5<-san.sims$Sim976
 #sac$sim50<-sac.sims$Sim715
 san$sim50<-san.sims$Sim581
 #sac$sim95<-sac.sims$Sim175
 san$sim95<-san.sims$Sim185
 
 #crops<-c("Almond_StudyArea", "Walnuts_StudyArea","Tomatoes_StudyArea", "Corn_StudyArea","Cotton_StudyArea", "Pistachios_StudyArea")

 # five_sac_sub <- sac[sac$sim5%in% crops, ]
 # five_san_sub <- san[san$sim5%in% crops, ]
 # fields<-rbind(five_san_sub, five_sac_sub)
 fields<-san[san$sim5%in% all_crops, ]
 fields_f <- fortify(fields, region = "ID")
 colnames(fields_f)[6]<-'ID'
 fields_fin <-merge(fields_f, fields@data,
                    by = "ID")
 sj_f <- fortify(sj, region = "OBJECTID")
 colnames(sj_f)[6]<-'ID'
 
 p1<-ggplot(data = fields_fin, aes(x=long, y=lat, group=group)) +
   geom_polygon(fill="#69b3a2") +
   geom_path(color = "white", size = 0.1) +
   geom_polygon(data = sj_f, aes(x = long, y = lat), fill = "lightblue") +
   # scale_fill_manual("#69b3a2")+
  coord_equal(ylim=c(1940000,1952500), xlim=c(-2155000,-2142000)) +
   theme(panel.background=element_blank())+
   theme(panel.background= element_rect(color="black")) +
   theme(axis.title = element_blank(),
         axis.text = element_blank()) +
   labs(title = "5th Percentile Area: 3728 Acres ")
 p1
 
 
 # fifty_sac_sub <- sac[sac$sim50%in% crops, ]
 # fifty_san_sub <- san[san$sim50%in% crops, ]
 # fields<-rbind(fifty_san_sub, fifty_sac_sub)
 fields<-san[san$sim50%in% all_crops, ]
 fields_f <- fortify(fields, region = "ID")
 colnames(fields_f)[6]<-'ID'
 fields_fin <-merge(fields_f, fields@data,
       by = "ID")
 sj_f <- fortify(sj, region = "OBJECTID")
 colnames(sj_f)[6]<-'ID'
 
  p2<-ggplot(data = fields_fin, aes(x=long, y=lat, group=group)) +
   geom_polygon(fill="#69b3a2") +
    geom_path(color = "white", size = 0.1) +
    geom_polygon(data = sj_f, aes(x = long, y = lat), fill = "lightblue") +
    # scale_fill_manual("#69b3a2")+
    coord_equal(ylim=c(1940000,1952500), xlim=c(-2155000,-2142000)) +
    theme(panel.background=element_blank())+
    theme(panel.background= element_rect(color="black")) +
    theme(axis.title = element_blank(),
          axis.text = element_blank()) +
    labs(title = "Median Area: 4067 Acres")
  p2
  

  # ninefive_sac_sub <- sac[sac$sim95%in% crops, ]
  # ninvefive_san_sub <- san[san$sim95%in% crops, ]
  # fields<-rbind(ninvefive_san_sub, ninefive_sac_sub)
  fields<-san[san$sim95%in% all_crops, ]
  fields_f <- fortify(fields, region = "ID")
  colnames(fields_f)[6]<-'ID'
  fields_fin <-merge(fields_f, fields@data,
                     by = "ID")
  sj_f <- fortify(sj, region = "OBJECTID")
  colnames(sj_f)[6]<-'ID'
  
  p3<-ggplot(data = fields_fin, aes(x=long, y=lat, group=group)) +
    geom_polygon(fill="#69b3a2") +
    geom_path(color = "white", size = 0.1) +
    geom_polygon(data = sj_f, aes(x = long, y = lat), fill = "lightblue") +
    # scale_fill_manual("#69b3a2")+
    coord_equal(ylim=c(1940000,1952500), xlim=c(-2155000,-2142000)) +
    theme(panel.background=element_blank())+
    theme(panel.background= element_rect(color="black")) +
    theme(axis.title = element_blank(),
          axis.text = element_blank()) +
    labs(title = "95th Percentile Area: 4409 Acres")
  p3
  
  #add the deterministic snapshot
  print(list.files(path=root_data_in, pattern='.tif$', all.files=TRUE, full.names=FALSE))
  deterministic <- file.path(root_data_in, 
                             list.files(path=root_data_in, pattern='.tif$', all.files=TRUE, full.names=FALSE))
  
 deterministic<-raster(deterministic[3])
  det_snap<-crop(deterministic, window)
  plot(det_snap)
  det_snap<-mask(det_snap, buff_1km)
  det_snap
  
  
  test_spdf <- as(det_snap, "SpatialPixelsDataFrame")
  test_df <- as.data.frame(test_spdf)
  colnames(test_df) <- c("value", "x", "y")
  var_x<-cellStats(det_snap, 'sum')
  var_x<-(var_x*900)*0.000247
  var_x
  
  
  p4<-ggplot() +
    geom_tile(data=test_df, aes(x=x, y=y, fill=value), alpha=0.8) + 
    scale_fill_gradient(low="white", high="#69b3a2", guide = FALSE)+
    geom_path(color = "white", size = 0.1) +
    geom_polygon(data = sj_f, aes(x = long, y = lat), fill = "lightblue") +
    coord_equal(ylim=c(1940000,1952500), xlim=c(-2155000,-2142000)) +
    theme(panel.background=element_blank())+
    theme(panel.background= element_rect(color="black")) +
    theme(axis.title = element_blank(),
          axis.text = element_blank()) +
    labs(title = "Deterministic Area: 5683 Acres")
  p4
  
  
  
  #add the reference map
  ca_dir<- file.path(root_data_in, "ca_counties")
  ca <- readOGR(dsn =  ca_dir, layer = "CA_Counties_TIGER2016")
  ca<-spTransform(ca,crs(vernal))
  ca$ID<-1:nrow(ca)
  ca.sub<-ca[ca$NAME == 'San Joaquin',]
  ca<-aggregate(ca, dissolve = TRUE)
  plot(ca)
  plot(ca.sub, add=T, col = "lightgrey")
  plot(vern.sub, add=T, col = "lightblue")
  plot(sj, add=T, col = "red")

  ca<-st_as_sf(ca)
  ca.sub<-st_as_sf(ca.sub)
  sj<-st_as_sf(sj)
  
  vern.sub<-st_as_sf(vern.sub)
  
  vern.sub.bb<-st_as_sfc(st_bbox(vern.sub))
  sj.bb<-st_as_sfc(st_bbox(sj))
  ca.sub.bb<-st_as_sfc(st_bbox(ca.sub))
 
  
  ref1 = ggplot() + 
    geom_sf(data = ca, fill = "white") + 
    geom_sf(data = ca.sub, fill = "lightgrey") +
    geom_sf(data = vern.sub, fill = "lightblue") +
    geom_sf(data = sj, fill = "lightblue") + 
    geom_sf(data = sj.bb, fill = NA, color = "red", size = 0.7) +
    theme_void()
  
  ref1
  
  ref2 = ggplot() + 
    geom_sf(data = ca.sub, fill = "lightgrey") +
    geom_sf(data = sj, fill = "lightblue") + 
    geom_sf(data = sj.bb, fill = NA, color = "red", size = 0.7) +
    theme_void()
  ref2
  
  ref<-plot_grid(ref1, ref2, nrow=1, rel_widths = c(0.5, 0.5))
  ref
  # first_row = plot_grid(histy, labels = c('Total Areas of Bifenthrin Crops within a 1km Buffer of a single Vernal Pool'))
  # second_row = plot_grid(ref, nrow = 1)
plot_grid(p2,p4, nrow = 1)
 plot_grid(p1,p3, nrow = 1)

#  gg_inset_map1 = ggdraw() +
#    draw_plot(histy, x = 1, y = 1, width = 0.8, height = 0.2)+
#    draw_plot(ref, x = 1, y = 0.5, width = 0.3, height = 0.3)+
#    draw_plot(row3, x = 1, y = 0.25, width = 0.3, height = 0.3)+
#    draw_plot(row4, x = 0.25, y = 0.25, width = 0.3, height = 0.3)
# gg_inset_map1
 
##Figure S2, Boxplot----
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
  
  mad<-mean(fin_total_sum_areas$Total)
  
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
  
  mer<-mean(fin_total_sum_areas$Total)
  
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
  sac<-mean(fin_total_sum_areas$Total)
  
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
  
  san<-mean(fin_total_sum_areas$Total)
  
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
  
  stan<-mean(fin_total_sum_areas$Total)
  
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
  
  
  
  #sum areas
  sum(mad+mer+san+sac+stan)*0.00024711
  