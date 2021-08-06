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

#madera
crop<-sum(mad[1,2:30])*0.000247105 #acreage of bifenthrin crops
print(crop)
tot<-1378353
print((crop/tot)*100)

#merced
crop<-sum(mer[1,2:30])*0.000247105 #acreage of bifenthrin crops
print(crop)
tot<-1266690
print((crop/tot)*100)


#sac
crop<-sum(sac[1,2:30])*0.000247105 #acreage of bifenthrin crops
print(crop)
tot<-636576
print((crop/tot)*100)

#san
crop<-sum(san[1,2:30])*0.000247105 #acreage of bifenthrin crops
print(crop)
tot<-913851
print((crop/tot)*100)

#stan
crop<-sum(stan[1,2:30])*0.000247105 #acreage of bifenthrin crops
print(crop)
tot<-969352
print((crop/tot)*100)


tot_all/5164822



####create histograms----
#read in ca counties
ca_dir = file.path(root_data_in, "/ca_counties")
ca <- readOGR(dsn =  ca_dir, layer = "CA_Counties_TIGER2016")
ca.sub<-ca[ca$NAME == 'Merced' | ca$NAME == 'Madera' | ca$NAME == 'Sacramento' | ca$NAME == 'Stanislaus' | ca$NAME == 'San Joaquin',] 
plot(ca.sub)

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

crops<-c("Almond_StudyArea", "Walnuts_StudyArea","Tomatoes_StudyArea", "Corn_StudyArea","Cotton_StudyArea", "Pistachios_StudyArea")


#san joaquin
hist_data<-as.data.frame(matrix(data=0,nrow=1000,ncol=2)) 
colnames(hist_data)[1]<-'BifenthrinCropArea'
colnames(hist_data)[2]<-'Sim'
hist_data[,2]<-colnames(san.sims)[2:1001]
for (sim in 2:ncol(san.sims)){
  bif_crops_san<-san.sims[san.sims[,sim] %in% crops,1:sim]
  bif_crop_area_san<- field_areas_san[field_areas_san[,2] %in% bif_crops_san[,1],]
  hist_data[sim-1,1] <-sum(bif_crop_area_san[,1])*0.00024711
}

quantile_sims<-as.data.frame(quantile(hist_data[,1], probs = c(0.05,0.5,0.95), names=F))
colnames(quantile_sims)[1]<-'area'
#quantile_sims[,1]<-round(quantile_sims[,1], 2)

histysj<-ggplot(hist_data, aes(x=BifenthrinCropArea)) + 
  geom_histogram(binwidth=100, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  geom_vline(xintercept=184700 , color="black", linetype="dashed", size=1)+
  xlab("Total Area of 6 Major Bifenthrin Crops (Sum of Acres)") + 
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
histysj

#corn, cotton, almond, walnuts, tomatoes, pistachios
SJ_d<-703.20+0+2838.11+4171.92+1474.87+5.33
SJ_p<-295901



