simulate_start_time <- Sys.time()
print("stepping into 05edit_and_update_manuscript_figures.R")
print(Sys.time())



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

