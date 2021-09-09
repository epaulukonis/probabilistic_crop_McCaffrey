simulate_start_time <- Sys.time()
print("stepping into 05edit_and_update_manuscript_figures.R")
print(Sys.time())



####calculate area tables----
total_crop_and_field_area_mad<-file.path(root_data_out, "total_crop_and_field_area_mad.csv")
mad<-read.csv(total_crop_and_field_area_mad)

total_crop_and_field_area_mer<-file.path(root_data_out, "total_crop_and_field_area_mer.csv")
mer<-read.csv(total_crop_and_field_area_mer)

total_crop_and_field_area_sac<-file.path(root_data_out, "total_crop_and_field_area_sac.csv")
sac<-read.csv(total_crop_and_field_area_sac)

total_crop_and_field_area_san<-file.path(root_data_out, "total_crop_and_field_area_san.csv")
san<-read.csv(total_crop_and_field_area_san)

total_crop_and_field_area_stan<-file.path(root_data_out, "total_crop_and_field_area_stan.csv")
stan<-read.csv(total_crop_and_field_area_stan)


tot_all<-(sum(mad[1,2:30] + mer[1,2:30] + sac[1,2:30] + san[1,2:30] + stan[1,2:30]))*0.000247105 
crop_all<-(sum(mad[1,2:29] + mer[1,2:29] + sac[1,2:29] + san[1,2:29] + stan[1,2:29]))*0.000247105 

#madera
crop<-sum(mad[1,2:30])*0.000247105 #acreage of bifenthrin crops
print(crop)
tot<-1378353
print((crop/tot)*100) #percent

#merced
crop<-sum(mer[1,2:30])*0.000247105 #acreage of bifenthrin crops
print(crop)
tot<-1266690
print((crop/tot)*100) #percent


#sac
crop<-sum(sac[1,2:30])*0.000247105 #acreage of bifenthrin crops
print(crop)
tot<-636576
print((crop/tot)*100) #percent

#san
crop<-sum(san[1,2:30])*0.000247105 #acreage of bifenthrin crops
print(crop)
tot<-913851
print((crop/tot)*100) #percent

#stan
crop<-sum(stan[1,2:30])*0.000247105 #acreage of bifenthrin crops
print(crop)
tot<-969352
print((crop/tot)*100) #percent


tot_all/5164822




####create histograms----
#read in ca counties

counties_trans<-readRDS("C:/Users/epauluko/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/GitHub/probabilistic_crop_McCaffrey/data_out/counties_trans.rds")

ca_dir = file.path(root_data_in, "/ca_counties")
ca <- readOGR(dsn =  ca_dir, layer = "CA_Counties_TIGER2016")
ca.sub<-ca[ca$NAME == 'Merced' | ca$NAME == 'Madera' | ca$NAME == 'Sacramento' | ca$NAME == 'Stanislaus' | ca$NAME == 'San Joaquin',] 
plot(ca.sub)
ca.sub.f <- fortify(ca.sub, region = "GEOID")
colnames(ca.sub.f)[6]<-'ID'


mad<-gBuffer(counties_trans[[1]], byid=T, width=0)
mer<-gBuffer(counties_trans[[2]], byid=T, width=0)
sac<-gBuffer(counties_trans[[3]], byid=T, width=0)
san<-gBuffer(counties_trans[[4]], byid=T, width=0)
stan<-gBuffer(counties_trans[[5]], byid=T, width=0)

sim_mat_san<-file.path(root_data_out, "simulation_matrix_san.csv")
sim_mat_stan<-file.path(root_data_out, "simulation_matrix_stan.csv")
sim_mat_sac<-file.path(root_data_out, "simulation_matrix_sac.csv")
sim_mat_mer<-file.path(root_data_out, "simulation_matrix_mer.csv")
sim_mat_mad<-file.path(root_data_out, "simulation_matrix_mad.csv")

sim_mat_sac<-read.csv(sim_mat_sac)[,-1]
sim_mat_san<-read.csv(sim_mat_san)[,-1]
sim_mat_stan<-read.csv(sim_mat_stan)[,-1]
sim_mat_mer<-read.csv(sim_mat_mer)[,-1]
sim_mat_mad<-read.csv(sim_mat_mad)[,-1]

field_areas_sac<-file.path(root_data_out, "field_areas_sac.csv")
field_areas_sac<-read.csv(field_areas_sac)[,-1]
field_areas_san<-file.path(root_data_out, "field_areas_san.csv")
field_areas_san<-read.csv(field_areas_san)[,-1]
field_areas_stan<-file.path(root_data_out, "field_areas_stan.csv")
field_areas_stan<-read.csv(field_areas_stan)[,-1]
field_areas_mer<-file.path(root_data_out, "field_areas_mer.csv")
field_areas_mer<-read.csv(field_areas_mer)[,-1]
field_areas_mad<-file.path(root_data_out, "field_areas_mad.csv")
field_areas_mad<-read.csv(field_areas_mad)[,-1]

san$ID<-1:nrow(san)
stan$ID<-1:nrow(stan)
sac$ID<-1:nrow(sac)
mer$ID<-1:nrow(mer)
mad$ID<-1:nrow(mad)
sac.df <- as.data.frame(sac)
san.df <- as.data.frame(san)
stan.df <- as.data.frame(stan)
mer.df <- as.data.frame(mer)
mad.df <- as.data.frame(mer)

sac.df.f<-sac.df[sac.df$ID %in% sim_mat_sac$ID,] #remove any rows that may not be present in the final sim
sac.sims<-sim_mat_sac[sim_mat_sac$ID %in% sac.df.f$ID,]
san.df.f<-san.df[san.df$ID %in% sim_mat_san$ID,] #remove any rows that may not be present in the final sim
san.sims<-sim_mat_san[sim_mat_san$ID %in% san.df.f$ID,]
mer.df.f<-mer.df[mer.df$ID %in% sim_mat_mer$ID,] #remove any rows that may not be present in the final sim
mer.sims<-sim_mat_mer[sim_mat_mer$ID %in% mer.df.f$ID,]
stan.df.f<-stan.df[sac.df$ID %in% sim_mat_stan$ID,] #remove any rows that may not be present in the final sim
stan.sims<-sim_mat_stan[sim_mat_stan$ID %in% stan.df.f$ID,]
mad.df.f<-mad.df[mad.df$ID %in% sim_mat_mad$ID,] #remove any rows that may not be present in the final sim
mad.sims<-sim_mat_mad[sim_mat_mad$ID %in% mad.df.f$ID,]

m_crops<-c("Almond_StudyArea", "Walnuts_StudyArea","Tomatoes_StudyArea", "Corn_StudyArea","Cotton_StudyArea", "Pistachios_StudyArea")
all_crops<-c("Alfalfa_StudyArea","Almond_StudyArea","Cabbage_StudyArea", "Cantaloupes_StudyArea","Corn_StudyArea",
             "Cotton_StudyArea", "Cucumbers_StudyArea","DryBeans_StudyArea","Eggplants_StudyArea","Fallow_Idle_StudyArea",
             "Grapes_StudyArea","Honeydew_Melons_StudyArea", "Lettuce_StudyArea", "Misc_Vegs_Fruits_StudyArea", "Oats_StudyArea", 
             "Oranges_StudyArea","Pears_StudyArea","Pecans_StudyArea","Peppers_StudyArea","Pistachio_StudyArea","Pomegranates_StudyArea",
             "Potatoes_StudyArea", "Pumpkins_StudyArea","Soybeans_StudyArea","Squash_StudyArea","Sweet_potatoes_StudyArea",
             "Tomatoes_StudyArea", "Walnuts_StudyArea","Watermelons_StudyArea")


###madera----
hist_datama<-as.data.frame(matrix(data=0,nrow=1000,ncol=3)) 
colnames(hist_datama)[1]<-'BifenthrinCropArea'
colnames(hist_datama)[2]<-'Sim'
colnames(hist_datama)[3]<-'County'
hist_datama[,2]<-colnames(mad.sims)[2:1001]
hist_datama[,3]<-'Madera'
for (sim in 2:ncol(mad.sims)){
  bif_crops_mad<-mad.sims[mad.sims[,sim] %in% all_crops,1:2]
  bif_crop_area_mad<- field_areas_mad[field_areas_mad[,2] %in% bif_crops_mad[,1],]
  hist_datama[sim-1,1] <-sum(bif_crop_area_mad[,1])*0.00024711
}

# quantile_sims<-as.data.frame(quantile(hist_data[,1], probs = c(0.05,0.5,0.95), names=F))
# colnames(quantile_sims)[1]<-'area'
#quantile_sims[,1]<-round(quantile_sims[,1], 2)

#add deterministic and probabilistic lines
ma_d<-340621
ma_p<-187618
ma_m<-186030

histyma<-ggplot(hist_datama, aes(x=BifenthrinCropArea)) + 
  geom_histogram(binwidth=25, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  # geom_vline(xintercept=ma_d , color="red", linetype="dashed", size=1)+
  geom_vline(xintercept=ma_p , color="blue", linetype="dashed", size=1)+
  geom_vline(xintercept=ma_m , color="black", linetype="dashed", size=1)+
  # # scale_x_log10()+
  #scale_x_continuous(breaks = seq(182000,341000,50000))+
  xlab("Total Area of Bifenthrin Crops (Sum of Acres)") + 
  ylab("Density")+
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
histyma


# ma_areas<-ggplot(hist_datama, aes(x=County, y=BifenthrinCropArea, fill=County)) + 
#   geom_hline(yintercept=ma_d , color="red", linetype="dashed", size=1)+
#   geom_hline(yintercept=ma_p , color="blue", linetype="dashed", size=1)+
#   geom_hline(yintercept=ma_m , color="black", linetype="dashed", size=1)+
#   geom_boxplot()
# ma_areas

mad.sub<-ca.sub[ca.sub$NAME == 'Madera',]
mad.sub.f <- fortify(mad.sub, region = "GEOID")
colnames(mad.sub.f)[6]<-'ID'

p1<-ggplot(data = ca.sub.f, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white") +
  geom_polygon(data = mad.sub.f, aes(x = long, y = lat), fill = "#69b3a2") +
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black")) +
  theme(axis.title = element_blank(),
        axis.text = element_blank()) 
p1

plot_grid(histyma,p1, align = "h", nrow = 1, ncol=2, rel_widths = c(2, 1))


###merced----
hist_datame<-as.data.frame(matrix(data=0,nrow=1000,ncol=3)) 
colnames(hist_datame)[1]<-'BifenthrinCropArea'
colnames(hist_datame)[2]<-'Sim'
colnames(hist_datame)[3]<-'County'
hist_datame[,2]<-colnames(mer.sims)[2:1001]
hist_datame[,3]<-'Merced'
for (sim in 2:ncol(mer.sims)){
  bif_crops_mer<-mer.sims[mer.sims[,sim] %in% all_crops,1:2]
  bif_crop_area_mer<- field_areas_mer[field_areas_mer[,2] %in% bif_crops_mer[,1],]
  hist_datame[sim-1,1] <-sum(bif_crop_area_mer[,1])*0.00024711
}

# quantile_sims<-as.data.frame(quantile(hist_data[,1], probs = c(0.05,0.5,0.95), names=F))
# colnames(quantile_sims)[1]<-'area'
#quantile_sims[,1]<-round(quantile_sims[,1], 2)

#add deterministic and probabilistic lines
me_p<-246856 
me_m<-240266 
histyme<-ggplot(hist_datame, aes(x=BifenthrinCropArea)) + 
  geom_histogram(binwidth=50, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  geom_vline(xintercept=me_m , color="black", linetype="dashed", size=1)+
  geom_vline(xintercept=me_p , color="blue", linetype="dashed", size=1)+
  xlab("Total Area of Bifenthrin Crops (Sum of Acres)") + 
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
histyme

mer.sub<-ca.sub[ca.sub$NAME == 'Merced',]
mer.sub.f <- fortify(mer.sub, region = "GEOID")
colnames(mad.sub.f)[6]<-'ID'

p1<-ggplot(data = ca.sub.f, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white") +
  geom_polygon(data = mer.sub.f, aes(x = long, y = lat), fill = "#69b3a2") +
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black")) +
  theme(axis.title = element_blank(),
        axis.text = element_blank()) 
p1

plot_grid(histyme,p1, align = "h", nrow = 1, ncol=2, rel_widths = c(2, 1))

###sacramento----
hist_datasa<-as.data.frame(matrix(data=0,nrow=1000,ncol=3)) 
colnames(hist_datasa)[1]<-'BifenthrinCropArea'
colnames(hist_datasa)[2]<-'Sim'
colnames(hist_datasa)[3]<-'County'
hist_datasa[,2]<-colnames(sac.sims)[2:1001]
hist_datasa[,3]<-'Sacramento'
for (sim in 2:ncol(sac.sims)){
  bif_crops_sac<-sac.sims[sac.sims[,sim] %in% all_crops,1:2]
  bif_crop_area_sac<- field_areas_sac[field_areas_sac[,2] %in% bif_crops_sac[,1],]
  hist_datasa[sim-1,1] <-sum(bif_crop_area_sac[,1])*0.00024711
}

# quantile_sims<-as.data.frame(quantile(hist_data[,1], probs = c(0.05,0.5,0.95), names=F))
# colnames(quantile_sims)[1]<-'area'
#quantile_sims[,1]<-round(quantile_sims[,1], 2)

#add deterministic and probabilistic lines
Sa_m<-40657 
Sa_p<-59465 
histysa<-ggplot(hist_datasa, aes(x=BifenthrinCropArea)) + 
  geom_histogram(binwidth=200, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  geom_vline(xintercept=Sa_m , color="black", linetype="dashed", size=1)+
   geom_vline(xintercept=Sa_p , color="blue", linetype="dashed", size=1)+
  xlab("Total Area of Bifenthrin Crops (Sum of Acres)") + 
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
histysa

sac.sub<-ca.sub[ca.sub$NAME == 'Sacramento',]
sac.sub.f <- fortify(sac.sub, region = "GEOID")
colnames(sac.sub.f)[6]<-'ID'

p1<-ggplot(data = ca.sub.f, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white") +
  geom_polygon(data = sac.sub.f, aes(x = long, y = lat), fill = "#69b3a2") +
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black")) +
  theme(axis.title = element_blank(),
        axis.text = element_blank()) 
p1

plot_grid(histysa,p1, align = "h", nrow = 1, ncol=2, rel_widths = c(2, 1))


###san joaquin----
hist_datasj<-as.data.frame(matrix(data=0,nrow=1000,ncol=3)) 
colnames(hist_datasj)[1]<-'BifenthrinCropArea'
colnames(hist_datasj)[2]<-'Sim'
colnames(hist_datasj)[3]<-'County'
hist_datasj[,2]<-colnames(san.sims)[2:1001]
hist_datasj[,3]<-'San joaquin'
for (sim in 2:ncol(san.sims)){
  bif_crops_san<-san.sims[san.sims[,sim] %in% all_crops,1:2]
  bif_crop_area_san<- field_areas_san[field_areas_san[,2] %in% bif_crops_san[,1],]
  hist_datasj[sim-1,1] <-sum(bif_crop_area_san[,1])*0.00024711
}

# quantile_sims<-as.data.frame(quantile(hist_data[,1], probs = c(0.05,0.5,0.95), names=F))
# colnames(quantile_sims)[1]<-'area'
#quantile_sims[,1]<-round(quantile_sims[,1], 2)

#add deterministic and probabilistic lines
SJ_m<-275911 
SJ_p<-295901 
histysj<-ggplot(hist_datasj, aes(x=BifenthrinCropArea)) + 
  geom_histogram(binwidth=150, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  geom_vline(xintercept=SJ_m , color="black", linetype="dashed", size=1)+
  geom_vline(xintercept=SJ_p , color="blue", linetype="dashed", size=1)+
  xlab("Total Area of Bifenthrin Crops (Sum of Acres)") + 
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
histysj

san.sub<-ca.sub[ca.sub$NAME == 'San Joaquin',]
san.sub.f <- fortify(san.sub, region = "GEOID")
colnames(san.sub.f)[6]<-'ID'

p1<-ggplot(data = ca.sub.f, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white") +
  geom_polygon(data = san.sub.f, aes(x = long, y = lat), fill = "#69b3a2") +
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black")) +
  theme(axis.title = element_blank(),
        axis.text = element_blank()) 
p1

plot_grid(histysj,p1, align = "h", nrow = 1, ncol=2, rel_widths = c(2, 1))

####stanislaus----
hist_datast<-as.data.frame(matrix(data=0,nrow=1000,ncol=3)) 
colnames(hist_datast)[1]<-'BifenthrinCropArea'
colnames(hist_datast)[2]<-'Sim'
colnames(hist_datast)[3]<-'County'
hist_datast[,2]<-colnames(stan.sims)[2:1001]
hist_datast[,3]<-'Stanislaus'
for (sim in 2:ncol(stan.sims)){
  bif_crops_stan<-stan.sims[stan.sims[,sim] %in% all_crops,1:2]
  bif_crop_area_stan<- field_areas_stan[field_areas_stan[,2] %in% bif_crops_stan[,1],]
  hist_datast[sim-1,1] <-sum(bif_crop_area_stan[,1])*0.00024711
}

# quantile_sims<-as.data.frame(quantile(hist_data[,1], probs = c(0.05,0.5,0.95), names=F))
# colnames(quantile_sims)[1]<-'area'
#quantile_sims[,1]<-round(quantile_sims[,1], 2)

#add deterministic and probabilistic lines
St_m<-198348 
St_p<-206026 
histyst<-ggplot(hist_datast, aes(x=BifenthrinCropArea)) + 
  geom_histogram(binwidth=100, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  geom_vline(xintercept=St_m , color="black", linetype="dashed", size=1)+
  geom_vline(xintercept=St_p , color="blue", linetype="dashed", size=1)+
  xlab("Total Area of Bifenthrin Crops (Sum of Acres)") + 
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
histyst

stan.sub<-ca.sub[ca.sub$NAME == 'Stanislaus',]
stan.sub.f <- fortify(stan.sub, region = "GEOID")
colnames(stan.sub.f)[6]<-'ID'

p1<-ggplot(data = ca.sub.f, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white") +
  geom_polygon(data = stan.sub.f, aes(x = long, y = lat), fill = "#69b3a2") +
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black")) +
  theme(axis.title = element_blank(),
        axis.text = element_blank()) 
p1

plot_grid(histyst,p1, align = "h", nrow = 1, ncol=2, rel_widths = c(2, 1))

####create boxplot with counties ----

##all area
hist_datama$AreaRatio <- 340621/hist_datama$BifenthrinCropArea
hist_datame$AreaRatio <- 479098/hist_datame$BifenthrinCropArea
hist_datasa$AreaRatio <- 192993/hist_datasa$BifenthrinCropArea
hist_datasj$AreaRatio <- 508653/hist_datasj$BifenthrinCropArea
hist_datast$AreaRatio <- 372095/hist_datast$BifenthrinCropArea

hist_data<-rbind(hist_datama,hist_datame,hist_datasa,hist_datasj,hist_datast)
bp_areas<-ggplot(hist_data, aes(x=County, y=AreaRatio, fill=County)) + 
  geom_boxplot()+
  xlab("County") + 
  ylab("Ratio of Deterministic Crop Area to Simulated Probabilistic Crop Areas")+
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
        axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
        legend.position = "none")
bp_areas


##vernal pool area
#we need to read in the deterministic raster and calculate the total area of crops within 1km of vernal pools by county
print(list.files(path=root_data_in, pattern='.tif$', all.files=TRUE, full.names=FALSE))
deterministic <- file.path(root_data_in, 
                           list.files(path=root_data_in, pattern='.tif$', all.files=TRUE, full.names=FALSE))

deterministic<-raster(deterministic[3])
plot(deterministic)

vernal<-spTransform(vernal,crs(crop_raster_stack[[1]]))
buff_1km<-gBuffer(vernal, byid = TRUE, width = 1000, capStyle="ROUND")
plot(buff_1km)

hist_datama_1k<-as.data.frame(matrix(data=0,nrow=1000,ncol=3)) 
colnames(hist_datama_1k)[1]<-'BifenthrinCropArea'
colnames(hist_datama_1k)[2]<-'Sim'
colnames(hist_datama_1k)[3]<-'County'
hist_datama_1k[,2]<-colnames(mad.sims)[2:1001]
hist_datama_1k[,3]<-'Madera'
mad_c<-crop(mad, buff_1km)
for (sim in 2:ncol(mad.sims)){
  bif_crops_mad<-mad.sims[mad.sims[,sim] %in% all_crops,1:2]
  bif_crop_area_mad<- field_areas_mad[field_areas_mad[,2] %in% bif_crops_mad[,1],]
  mad_vp<-mad_c[mad_c$ID %in% bif_crop_area_mad$ID,]
  mad_vp$acres<-bif_crop_area_mad[bif_crop_area_mad$ID %in% mad_vp$ID, 1]
  hist_datama_1k[sim-1,1] <-round(sum(mad_vp$acres)*0.00024711, 2)
}

hist_datame_1k<-as.data.frame(matrix(data=0,nrow=1000,ncol=3)) 
colnames(hist_datame_1k)[1]<-'BifenthrinCropArea'
colnames(hist_datame_1k)[2]<-'Sim'
colnames(hist_datame_1k)[3]<-'County'
hist_datame_1k[,2]<-colnames(mer.sims)[2:1001]
hist_datame_1k[,3]<-'Merced'
mer_c<-crop(mer, buff_1km)
for (sim in 2:ncol(mer.sims)){
  bif_crops_mer<-mer.sims[mer.sims[,sim] %in% all_crops,1:2]
  bif_crop_area_mer<- field_areas_mer[field_areas_mer[,2] %in% bif_crops_mer[,1],]
  mer_vp<-mer_c[mer_c$ID %in% bif_crop_area_mer$ID,]
  mer_vp$acres<-bif_crop_area_mer[bif_crop_area_mer$ID %in% mer_vp$ID, 1]
  hist_datame_1k[sim-1,1] <-round(sum(mer_vp$acres)*0.00024711, 2)
}

hist_datasa_1k<-as.data.frame(matrix(data=0,nrow=1000,ncol=3)) 
colnames(hist_datasa_1k)[1]<-'BifenthrinCropArea'
colnames(hist_datasa_1k)[2]<-'Sim'
colnames(hist_datasa_1k)[3]<-'County'
hist_datasa_1k[,2]<-colnames(sac.sims)[2:1001]
hist_datasa_1k[,3]<-'San Joaquin'
sac_c<-crop(sac, buff_1km)
for (sim in 2:ncol(sac.sims)){
  bif_crops_sac<-sac.sims[sac.sims[,sim] %in% all_crops,1:2]
  bif_crop_area_sac<- field_areas_sac[field_areas_sac[,2] %in% bif_crops_sac[,1],]
  sac_vp<-sac_c[sac_c$ID %in% bif_crop_area_sac$ID,]
  sac_vp$acres<-bif_crop_area_sac[bif_crop_area_sac$ID %in% sac_vp$ID, 1]
  hist_datasa_1k[sim-1,1] <-round(sum(sac_vp$acres)*0.00024711, 2)
}

hist_datasj_1k<-as.data.frame(matrix(data=0,nrow=1000,ncol=3)) 
colnames(hist_datasj_1k)[1]<-'BifenthrinCropArea'
colnames(hist_datasj_1k)[2]<-'Sim'
colnames(hist_datasj_1k)[3]<-'County'
hist_datasj_1k[,2]<-colnames(san.sims)[2:1001]
hist_datasj_1k[,3]<-'San Joaquin'
san_c<-crop(san, buff_1km)
for (sim in 2:ncol(san.sims)){
  bif_crops_san<-san.sims[san.sims[,sim] %in% all_crops,1:2]
  bif_crop_area_san<- field_areas_san[field_areas_san[,2] %in% bif_crops_san[,1],]
  san_vp<-san_c[san_c$ID %in% bif_crop_area_san$ID,]
  san_vp$acres<-bif_crop_area_san[bif_crop_area_san$ID %in% san_vp$ID, 1]
  hist_datasj_1k[sim-1,1] <-round(sum(san_vp$acres)*0.00024711, 2)
}

hist_datast_1k<-as.data.frame(matrix(data=0,nrow=1000,ncol=3)) 
colnames(hist_datast_1k)[1]<-'BifenthrinCropArea'
colnames(hist_datast_1k)[2]<-'Sim'
colnames(hist_datast_1k)[3]<-'County'
hist_datast_1k[,2]<-colnames(stan.sims)[2:1001]
hist_datast_1k[,3]<-'San Joaquin'
stan_c<-crop(stan, buff_1km)
for (sim in 2:ncol(stan.sims)){
  bif_crops_stan<-stan.sims[stan.sims[,sim] %in% all_crops,1:2]
  bif_crop_area_stan<- field_areas_stan[field_areas_stan[,2] %in% bif_crops_stan[,1],]
  stan_vp<-stan_c[stan_c$ID %in% bif_crop_area_stan$ID,]
  stan_vp$acres<-bif_crop_area_stan[bif_crop_area_stan$ID %in% stan_vp$ID, 1]
  hist_datast_1k[sim-1,1] <-round(sum(stan_vp$acres)*0.00024711, 2)
}


#deterministic

county_list<-list(mad,mer,sac,san,stan)
ext_vps_d<-function(x){
  
}

d_ma<-crop(deterministic,mad)
d_ma_vp<-crop(d_ma,buff_1km)
d_ma_vp<-mask(d_ma_vp, buff_1km)
plot(d_ma_vp)




####create table of 1km area vernal pool co-occurence----
##madera
#pull out a simulation and calculate field area of crops 
sim<-mad.sims[,c(1,501)]
sim<-as.data.frame(sim[sim$Sim500 %in% all_crops,])
bif_crop_area_mad<- field_areas_mad[field_areas_mad[,2] %in% sim[,1],]
mad_c<-crop(mad, buff_1km)
plot(mad_c)
mad_vp<-mad_c[mad_c$ID %in% bif_crop_area_mad$ID,]
mad_vp$acres<-bif_crop_area_mad[bif_crop_area_mad$ID %in% mad_vp$ID, 1]
mad_area <-round(sum(mad_vp$acres)*0.00024711, 2)
print(mad_area)

##merced
#pull out a simulation and calculate field area of crops 
sim<-mer.sims[,c(1,501)]
sim<-as.data.frame(sim[sim$Sim500 %in% all_crops,])
bif_crop_area_mer<- field_areas_mer[field_areas_mer[,2] %in% sim[,1],]
mer_c<-crop(mer, buff_1km)
plot(mer_c)
mer_vp<-mer_c[mer_c$ID %in% bif_crop_area_mer$ID,]
mer_vp$acres<-bif_crop_area_mer[bif_crop_area_mer$ID %in% mer_vp$ID, 1]
mer_area <-round(sum(mer_vp$acres)*0.00024711, 2)
print(mer_area)

##sacramento
#pull out a simulation and calculate field area of crops 
sim<-sac.sims[,c(1,501)]
sim<-as.data.frame(sim[sim$Sim500 %in% all_crops,])
bif_crop_area_sac<- field_areas_sac[field_areas_sac[,2] %in% sim[,1],]
sac_c<-crop(sac, buff_1km)
plot(sac_c)
sac_vp<-sac_c[sac_c$ID %in% bif_crop_area_sac$ID,]
sac_vp$acres<-bif_crop_area_sac[bif_crop_area_sac$ID %in% sac_vp$ID, 1]
sac_area <-round(sum(sac_vp$acres)*0.00024711, 2)
print(sac_area)

##san joaquin
#pull out a simulation and calculate field area of crops 
sim<-san.sims[,c(1,501)]
sim<-as.data.frame(sim[sim$Sim500 %in% all_crops,])
bif_crop_area_san<- field_areas_san[field_areas_san[,2] %in% sim[,1],]
san_c<-crop(san, buff_1km)
plot(san_c)
san_vp<-san_c[san_c$ID %in% bif_crop_area_san$ID,]
san_vp$acres<-bif_crop_area_san[bif_crop_area_san$ID %in% san_vp$ID, 1]
san_area <-round(sum(san_vp$acres)*0.00024711, 2)
print(san_area)

##stanislaus
#pull out a simulation and calculate field area of crops 
sim<-stan.sims[,c(1,501)]
sim<-as.data.frame(sim[sim$Sim500 %in% all_crops,])
bif_crop_area_stan<- field_areas_stan[field_areas_stan[,2] %in% sim[,1],]
stan_c<-crop(stan, buff_1km)
plot(stan_c)
stan_vp<-stan_c[stan_c$ID %in% bif_crop_area_stan$ID,]
stan_vp$acres<-bif_crop_area_stan[bif_crop_area_stan$ID %in% stan_vp$ID, 1]
stan_area <-round(sum(stan_vp$acres)*0.00024711, 2)
print(stan_area)






