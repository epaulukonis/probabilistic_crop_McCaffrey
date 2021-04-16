library(sp)
library(tiff)
library(rgeos)
library(rgdal)
library(raster)
library(dplyr)

if(Sys.info()[4]=="LZ2626UTPURUCKE"){
  wd <- file.path("c:", "git", "probabilistic_crop_McCaffrey")
}else{
  load('myEnvironment_prob_crop.RData')
  wd='C:/Users/epauluko/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Paulukonis_Documents/manuscript_probabilistic_crop'
}


##Step 1: extract and align the spatial data 
#read in list of tif files denoting probability of each crop
ddir1 = file.path(wd,"FinalCropsPt1")
setwd(ddir1)
crop_stack1<-list.files(pattern='.tif$', all.files=TRUE, full.names=FALSE)

ddir2 = file.path(wd,"/FinalCropsPt2")
setwd(ddir2)
crop_stack2<-list.files(pattern='.tif$', all.files=TRUE, full.names=FALSE)

#crop stack (takes two lines due to size)
crop_stacka<- stack(crop_stack1)
crop_stackb<- stack(crop_stack2)
crop_stackf<-stack(crop_stacka,crop_stackb) 

#read in clip of county fields
merced <- readOGR(dsn = "C:/Users/epauluko/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Paulukonis_Documents/manuscript_probabilistic_crop/prob_crop_data/cadwr_merced.shp")
plot(merced) #quick peek at the field layer at county level
mercedf <- spTransform(merced,crs(crop_stackf)) #transform crs of county polygon to crs of crop stack

#visually double check that everything aligns 
plot(crop_stacka[[5]]) 
plot(mercedf, add=T)

ex<-extent(mercedf)# get extent of county
crop_stackf<-crop(crop_stackf,ex) #crop the crop stack to the county polygon
extent(crop_stackf) #check that extents match
extent(mercedf)
crop_stackf<-stack(crop_stackf) #turn it back into a stack

crs(mercedf) # double check that crs is in m
area_f<- as.data.frame(area(mercedf)) #area of each field in meters
colnames(area_f)[1]<-'area_field'
area_f$ID<-1:15925  #area_f

##Step 2: Simulate field level probabilities
#extract to fields
datff<- extract(crop_stackf, mercedf, df=T) #this extracts the raster cell values by polygons to a df
sum_mat<-datff %>% group_by(ID) %>% summarise_all(funs(mean)) #summarize each crop by field to mean
sum_c <- apply(sum_mat[,c(2:30)], 1, sum)
sum_mat$NC<-round(1-sum_c,4) #add in column for non-crop
sum_mat<-na.omit(sum_mat)
mat_n<-as.data.frame(matrix(data=NA,nrow=nrow(sum_mat),ncol=1000))
colnames(mat_n)[1:1000]<-paste0("Sim",1:1000,"")
ID<-unique(sum_mat$ID)
mat_n<-cbind(ID,mat_n)

#function to convert the rasters to binary
fun_c <- function(x) {
  x[x>0] <- 1
  return(x)
}
out<-stack(calc(crop_stackf, fun_c))


#calculate the area (m2) of each type of raster (crop/non-crop) for each crop type
m<-list()
for (i in 1:29) {
  y <- out[[i]]
  m[[i]]<-as.matrix(tapply(area(y), y[], sum))
}

m_a<-lapply(m, `length<-`, max(lengths(m))) #change length of list to all match
area_c <- data.frame(matrix(unlist(m_a), nrow=length(m_a), byrow=TRUE)) #turn into workable dataframe for total crop area
NC<-area_c[1,1]
area_c<-as.data.frame(area_c[,2])
area_c[30,]<-NC
colnames(area_c)[1]<-'crop_total'
area_c[is.na(area_c)] <- 0 #add in 0 #this is the dataset containing the total crop areas for each crop
area_c$field_tot<-0
area_c<-as.data.frame(t(area_c))
colnames(area_c)<-colnames(sum_mat)[2:31] 

#dataset for updating the areas that matches ncol and nrow of mat_n
area_up<-sum_mat[,2:31]
area_up[,1:30]<-0
area_up<-as.data.frame(area_up) 

#see how long this takes
sleep_for_a_minute <- function() { Sys.sleep(60) }
start_time <- Sys.time()
sleep_for_a_minute()

for (j in 2:ncol(mat_n)){
  for (i in 1:nrow(mat_n)){
    out<-sum_mat[sum_mat$ID %in% mat_n[i,1],]
    var<-out[,2:31] #pull out crops
    var<-rbind(var, area_c)
    #var_n<-ifelse(var[3,] == var[2,], 0, (1-(var[3,]/var[2,]))*var[1,]) #if total crop area = sum of field area, automatically assign 0 prob
    var_n<-(1-(var[3,]/var[2,]))*var[1,]
    var_n<-rapply(var_n, function(x) ifelse(is.nan(x),0,x), how="replace" ) 
    var_n[var_n < 0] <- 0 #if any probs are neg, change to 0 (above ifelse was not working, this is alternative)
    probs<-as.numeric(var_n) 
    r1<-sample(30, size = 1, replace = TRUE, prob = probs)
    mat_n[i,j] <-colnames(out)[r1+1]
    field_a<-area_f[area_f$ID %in% mat_n[i,1],]
    area_up[i,]<-ifelse(mat_n[i,j] == names(area_up), field_a[,1], 0)
    area_c[2,]<-colSums(area_up)
  }
}

end_time <- Sys.time()
end_time - start_time
#add in verification after running