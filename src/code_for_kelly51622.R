###code for kelly - 5/16/22

root_data<-wd()
crop_data_dir = file.path(root_data) #just to keep code intact
root_data_out = file.path(root_data)

#get crop_raster_stack (probabilistic layers)
#unzip if needed
##Final Crops.zip is the zip file you gave me containing the probabilistic layers, so this is just whatever file you have of that output

unzip_dir_filename <- file.path(crop_data_dir, "Final Crops.zip")
print(unzip_dir_filename )
unzipped_dir <- file.exists(file.path(crop_data_dir, "Alfalfa_StudyArea.tif"))
if(!unzipped_dir){unzip(unzip_dir_filename, exdir=crop_data_dir)}

print(list.files(path=crop_data_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
crop_stack_files <- file.path(crop_data_dir, 
                              list.files(path=crop_data_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
crop_stack_allfiles <- crop_stack_files 
crop_raster_stack <- stack(crop_stack_allfiles)

#get vernal pool data froma Leah
vernal <- readOGR(dsn =  root_data_out, layer = "vp_vpfs_fCH_71FR7117")


#get simulations from the field sim output
sim_mat_san<-file.path(root_data_out, "simulation_matrix_san.csv")
sim_mat_stan<-file.path(root_data_out, "simulation_matrix_stan.csv")
sim_mat_sac<-file.path(root_data_out, "simulation_matrix_sac.csv")
sim_mat_mer<-file.path(root_data_out, "simulation_matrix_mer.csv")
sim_mat_mad<-file.path(root_data_out, "simulation_matrix_mad.csv")

#drop first column when reading
sim_mat_sac<-read.csv(sim_mat_sac)[,-1]
sim_mat_san<-read.csv(sim_mat_san)[,-1]
sim_mat_stan<-read.csv(sim_mat_stan)[,-1]
sim_mat_mer<-read.csv(sim_mat_mer)[,-1]
sim_mat_mad<-read.csv(sim_mat_mad)[,-1]


#this might take a second
county_names<-c('madera','merced','sacramento','sanjoaquin','stanislaus')

#here is where you read in the 'cadwr_fielddata.zip; unzip this to whatever your 'county_shp_dir' is
counties <- list.files(county_shp_dir, pattern="\\.shp$", full.names=TRUE)
counties <- lapply(counties, shapefile)
names(counties)<-county_names
counties_trans <- lapply(counties, function(x) spTransform(x,crs(crop_raster_stack)))

mad<-gBuffer(counties_trans[[1]], byid=T, width=0)
mer<-gBuffer(counties_trans[[2]], byid=T, width=0)
sac<-gBuffer(counties_trans[[3]], byid=T, width=0)
san<-gBuffer(counties_trans[[4]], byid=T, width=0)
stan<-gBuffer(counties_trans[[5]], byid=T, width=0)

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



vernal<-spTransform(vernal,crs(crop_raster_stack[[1]]))
buff_1km<-gBuffer(vernal, byid = TRUE, width = 1000, capStyle="ROUND")
plot(buff_1km) #check that there's a buffer around the vps (might have to zoom in)



all_crops<-c("Alfalfa_StudyArea","Almond_StudyArea","Cabbage_StudyArea", "Cantaloupes_StudyArea","Corn_StudyArea",
             "Cotton_StudyArea", "Cucumbers_StudyArea","DryBeans_StudyArea","Eggplants_StudyArea","Fallow_Idle_StudyArea",
             "Grapes_StudyArea","Honeydew_Melons_StudyArea", "Lettuce_StudyArea", "Misc_Vegs_Fruits_StudyArea", "Oats_StudyArea", 
             "Oranges_StudyArea","Pears_StudyArea","Pecans_StudyArea","Peppers_StudyArea","Pistachio_StudyArea","Pomegranates_StudyArea",
             "Potatoes_StudyArea", "Pumpkins_StudyArea","Soybeans_StudyArea","Squash_StudyArea","Sweet_potatoes_StudyArea",
             "Tomatoes_StudyArea", "Walnuts_StudyArea","Watermelons_StudyArea")




##here is where I calculate the area of one of the simulated field layers around 1km buffer for each VP
##I select the 500th simulation; for the purposes of the table we just need one example of the area when we do the sims

##madera
#pull out a simulation and calculate field area of crops 
sim<-mad.sims[,c(1,501)]
sim<-as.data.frame(sim[sim$Sim500 %in% all_crops,])
bif_crop_area_mad<- field_areas_mad[field_areas_mad[,2] %in% sim[,1],]
mad_c<-crop(mad, buff_1km)
#plot(mad_c)
mad_vp<-mad_c[mad_c$ID %in% bif_crop_area_mad$ID,]
mad_vp$acres<-bif_crop_area_mad[bif_crop_area_mad$ID %in% mad_vp$ID,]
mad_area <-round(sum(mad_vp$acres)*0.00024711, 2)
print(mad_area)


##merced
#pull out a simulation and calculate field area of crops 
sim<-mer.sims[,c(1,501)]
sim<-as.data.frame(sim[sim$Sim500 %in% all_crops,])
bif_crop_area_mer<- field_areas_mer[field_areas_mer[,2] %in% sim[,1],]
mer_c<-crop(mer, buff_1km)
#plot(mer_c)
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
#plot(sac_c)
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
#plot(san_c)
san_vp<-san_c[san_c$ID %in% bif_crop_area_san$ID,]
san_vp$acres<-bif_crop_area_san[bif_crop_area_san$ID %in% san_vp$ID, 1]
san_area <-round(sum(san_vp$acres)*0.00024711, 2)
print(san_area)

##stanislaus
#pull out a random simulation and calculate field area of crops 
sim<-stan.sims[,c(1,501)]
sim<-as.data.frame(sim[sim$Sim500 %in% all_crops,])
bif_crop_area_stan<- field_areas_stan[field_areas_stan[,2] %in% sim[,1],]
stan_c<-crop(stan, buff_1km)
#plot(stan_c)
stan_vp<-stan_c[stan_c$ID %in% bif_crop_area_stan$ID,]
stan_vp$acres<-bif_crop_area_stan[bif_crop_area_stan$ID %in% stan_vp$ID, 1]
stan_area <-round(sum(stan_vp$acres)*0.00024711, 2)
print(stan_area)

print(sum(mad_area,mer_area,sac_area,san_area,stan_area))

