###code for kelly - 5/16/22

root_data<-wd()
crop_data_dir = file.path(root_data) #just to keep code intact
root_data_out = file.path(root_data)

#get vernal pool data from Leah
vernal <- readOGR(dsn =  root_data_out, layer = "vp_vpfs_fCH_71FR7117")


##add the deterministic snapshot
print(list.files(path=root_data_in, pattern='.tif$', all.files=TRUE, full.names=FALSE))
deterministic <- file.path(root_data_in, 
                           list.files(path=root_data_in, pattern='.tif$', all.files=TRUE, full.names=FALSE))

deterministic<-raster(deterministic)

##add in counties to buffer the deterministic 
#this might take a second
county_names<-c('madera','merced','sacramento','sanjoaquin','stanislaus')

#here is where you read in the 'ca 5 counties; unzip this to whatever your 'county_shp_dir' is
counties <- list.files(county_shp_dir, pattern="\\.shp$", full.names=TRUE)
counties <- lapply(counties, shapefile)
names(counties)<-county_names
counties_trans <- lapply(counties, function(x) spTransform(x,crs(deterministic)))


##here's a rough idea of how you can 
county_list<-list()
for (county in 1:length(counties_trans)){
det_county<-crop(deterministic, counties_trans[[county]])
det_county_1km<-mask(det_county, buff_1km)
county_list[[county]]<-det_county_1km
}


spdf <- as(mask, "SpatialPixelsDataFrame")
get_area <- as.data.frame(spdf)
colnames(get_area) <- c("value", "x", "y")
var_x<-cellStats(get_area, 'sum')
var_x<-(var_x*900)*0.000247
var_x


