library(sp)
library(tiff)
library(rgeos)
library(rgdal)
library(raster)
library(dplyr)

if(Sys.info()[4]=="LZ2626UTPURUCKE"){
  wd <- file.path("c:", "git", "probabilistic_crop_McKaffrey")
}else{
  load('myEnvironment_prob_crop.RData')
  wd='C:/Users/epauluko/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Paulukonis_Documents/manuscript_probabilistic_crop'
}
#we'll be treating this like a database problem; i.e., using rows and columns of the rasters 

##Step 1: extract the spatial data (note that if you load the environment this work has been done for you)
#read in list of tif files denoting probability of each crop
ddir1 = file.path(wd,"FinalCropsPt1")
setwd(ddir1)
crop_stack1<-list.files(pattern='.tif$', all.files=TRUE, full.names=FALSE)

ddir2 = file.path(wd,"/FinalCropsPt2")
setwd(ddir2)
crop_stack2<-list.files(pattern='.tif$', all.files=TRUE, full.names=FALSE)


#for some reason this is the only way it will let me stack for now (I think size)
crop_stacka<- stack(crop_stack1)
crop_stackb<- stack(crop_stack2)
crop_stackf<-stack(crop_stacka,crop_stackb) 


#read in clip of merced county fields
merced <- readOGR(dsn = "C:/Users/epauluko/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Paulukonis_Documents/manuscript_probabilistic_crop/prob_crop_data/cadwr_merced.shp")
plot(merced) #quick peek at the field layer
crs(merced)
crs(crop_stackf)
mercedf <- spTransform(merced,crs(crop_stackf)) #transform crs of merced polygon
ex<-extent(mercedf) #clip to extent of merced

plot(crop_stacka[[5]]) 
plot(mercedf, add=T)   # check to make sure that the merced plot is there

crop_stackf<-crop(crop_stackf,ex) #crop the crop stack to the merced polygon
extent(crop_stackf) #check that extents match
extent(mercedf)
crop_stackf<-stack(crop_stackf) #turn it back into a stack
plot(crop_stackf[[2]])

##Step 2: assign new probabilities (start here if loading previous environment)
datff<- extract(crop_stackf, mercedf, df=T) #this extracts the raster cell values by polygons to a df
sum_mat<-datff %>% group_by(ID) %>% summarise_all(funs(mean)) #summarize each crop by field to mean

#let's create a mini dataset to work through
mini_dat<-sum_mat[1:10,]
sum_c <- apply(mini_dat[,c(2:30)], 1, sum)
mini_dat$NC<-1-sum_c #add in column for non-crop
mat_n<-as.data.frame(matrix(data=NA,nrow=nrow(mini_dat),ncol=1000)) #new empty data-frame to store new field values
colnames(mat_n)[1:1000]<-paste0("Sim",1:1000,"")
ID<-unique(mini_dat$ID)
mat_n<-cbind(ID,mat_n)

#let's get the total areas of each crop 
#function to convert the rasters to binary
fun_c <- function(x) {
  #x[is.na(x[])] <- 0 #don't do this because it expands the area
  x[x>0] <- 1
  return(x)
}
out<-stack(calc(crop_stackf, fun_c))
m<-list()
#calculate the area (m2) of each type of raster (crop/non-crop) for each crop type
for (i in 1:29) {
  y <- out[[i]]
  m[[i]]<-as.matrix(tapply(area(y), y[], sum))
}
#should output the total area (m2)for each crop as a list
# prct<-lapply(m, function(x) x[2]/sum(x)) #percent of landscape that is this crop  
# prct<-as.data.frame(unlist(prct)) #here is the percent of each crop
# prct$crop<-colnames(sum_mat)[2:30] #add names to the prct
m_a<-lapply(m, `length<-`, max(lengths(m))) #change length of list to all match
area_c <- data.frame(matrix(unlist(m_a), nrow=length(m_a), byrow=TRUE)) #turn into workable dataframe for total crop area
NC<-area_c[1,1]
area_c<-as.data.frame(area_c[,2])
area_c[30,]<-NC
colnames(area_c)[1]<-'C'
area_c$Crop<-colnames(mini_dat)[2:31] #add names to the list of crop area
area_c[is.na(area_c)] <- 0 #add in 0 #this is the dataset containing the total crop areas for each crop
area_c$field_tot<-0

#dataset for updating the areas that matches ncol and nrow of mat_n
area_up<-mini_dat
area_up[,2:31]<-0


#now let's see if we can get it to work for our mat_n and mini_dat
for (j in 2:ncol(mat_n)){ #by simulation
    for (i in 1:nrow(mat_n)){ #by ID
      out<-mini_dat[mini_dat$ID %in% mat_n[i,1],] #this pulls out the row of crop probabilities by ID
      var<-out[,2:31] #only probs
      probs<-as.numeric(var) #numeric for sample function
      r1<-sample(30, size = 1, replace = TRUE, prob = probs) #use IDs of each crop, and then the probs associated with crop
      mat_n[i,j] <-names(var[,r1]) #assign probs originally
      mat_n[i,j]<-ifelse(mat_n[i,j] == area_c$Crop, (1-(area_c$field_tot/area_c$C)), NA)
    }
  
  for (x in 2:ncol(area_up)){
  area_o<-area_f[area_f$ID %in% out[,1], ] 
  area_o<-area_o[,1]
  area_up[i,x]<-ifelse(out[,1] == area_up$ID & mat_n[i,j] == names(area_up), area_o, 0)
  area_c$field_tot<-colsums(area_up[,2:31])
  }
}
#I'm doing something wrong here; I'm guessing it doesn't like the indices I'm using 



########everything below is scraps/experimental
#this is the original working 
for (j in 2:ncol(mat_n)){
  for (i in 1:nrow(mat_n)){
    out<-min_dat[mini_dat$ID %in% mat_n[i,1],] 
    var<-out[,2:31] #pull out crops
    probs<-as.numeric(var)
    r1<-sample(30, size = 1, replace = TRUE, prob = probs)
    mat_n[i,j] <-names(var[,r1])
  }
  
}
 



#function over rows?
#how do we add in an updated probability based on area?
#1- (area if crop so far/total area of crop)


f_c <- m_c[m_c$Crop %in% mat_n[i,j],]
m_c[x,4] <-m_c[x,4] + f_c
#try for a single row...how does this work?
out<-mini_dat[mini_dat$ID %in% mat_n[1,1],]
area_c<-area_f[area_f$ID %in% out[,1], ] 
area_c<-area_c[,1]
var<-out[,2:31] #pull out crops
probs<-as.numeric(var)

r1<-sample(30, size = 1, replace = TRUE, prob = probs) #use IDs of each crop, and then the probs associated with crop
crp <-names(var[,r1]) #assign probs originally
f_c<-m_c[m_c$Crop %in% crp,]

m_c$area_field<-ifelse(f_c$Crop == m_c$Crop, area_c, NA)


m_c[x,4] <-m_c[x,4] + f_c
mat_n[i,j]<- 1 - (m_c[x,4]/m_c[x,2]) #update probs


prob_fun<-function(y){
  c<-m_c[m_c$Crop %in% names(y),] 
  c<-c[,2:3]
  n_prob<-1-(areac/c[,1])
  return(n_prob)
}

ara<-area_f[area_f$ID %in% out[,1], ] #this is the area of the field ID
mat_a<-ifelse(mat_n[i,x] == mat_a$crop, ara, NA) 

# c <- m_c[m_c$Crop %in%  mat_n[i,1],]
# c<-c[,2]
# n_prob<- 1-(sum(mat_a[,x])/c) #1 value
#probs<-as.numeric(var) 

#where id of mat_n == id area_f, insert area_f$area_field
#30 * 1000 - after we make assignment but before we move on to the next row,
#we'll update the areas for that crop
#so that the probs are updated by the ratio of area of crop x so far/total area of crop x
#once we finish our 1000th assignment, we take that field area and add it to the appropriate cell 
#after first field is done, then we add that field area to the appropriate crop area sim
#update probs here/add in area spreadsheet - this reads from line 109
#we'll have to convert the probs to 0 if total area crop x is full up by prob value
#1-ratio - you can see if rounding takes care of potential neg numbers, but other options as well 
#use IDs of each crop, and then the probs associated with crop
#excellent, this works
#next two tasks: 
#a) add in the area constraints (using Kelly's probabilities)
#b) put it in the for loop essentially the two step for loop will allow for us to update the area

#we can go ahead and use kelly's analysis to fill in missing acreage
#i.e., whatever percentage she has for the missing crops, use that to estimate those missing rows
#use kelly's estimates entirely; just percent * total acerage of cropland by county
#use all of kelly's areas, not CoA, s we can do in other states
#then figure to do a comparison between coa and kelly's estimate 


crs(mercedf) #check that crs is in m
area_f<- as.data.frame(area(mercedf)) #area of each field in meters
colnames(area_f)[1]<-'area_field'
area_f$ID<-1:15925 
#area_f is going to give us the area in m2 ##don't worry about this quite yet
#so instead of using NASS to verify the acreage of the simulations
#what we'll do instead is use the acres from the estimated probability


#this is for the entire dataset, which takes too long to run, so we'll break it up eventually
sum_c <- apply(sum_mat[,c(2:30)], 1, sum)
sum_mat$NC<-round(1-sum_c,4) #add in column for non-crop
sum_mat<-na.omit(sum_mat)
mat_n<-as.data.frame(matrix(data=NA,nrow=nrow(sum_mat),ncol=1000))
colnames(mat_n)[1:1000]<-paste0("Sim",1:1000,"")
ID<-unique(sum_mat$ID)
mat_n<-cbind(ID,mat_n)
#field areas x sims empty matrix, this is updated by 110-114

for (i in 1:nrow(mat_n)){
  for (x in 2:ncol(mat_n)){
    out<-sum_mat[sum_mat$ID %in% mat_n[i,1],] 
    var<-out[,2:31] #pull out crops
    probs<-as.numeric(var)
    #update probs here/add in area spreadsheet - this reads from line 109
    #we'll have to convert the probs to 0 if total area crop x is full up by prob value
    #1-ratio - you can see if rounding takes care of potential neg numbers, but other options as well 
    #use IDs of each crop, and then the probs associated with crop
    r1<-sample(30, size = 1, replace = TRUE, prob = probs)
    mat_n[i,x] <-names(var[,r1])
  }
  #30 * 1000 - after we make assignment but before we move on to the next row,
  #we'll update the areas for that crop
  #so that the probs are updated by the ratio of area of crop x so far/total area of crop x
  #once we finish our 1000th assignment, we take that field area and add it to the appropriate cell 
  #after first field is done, then we add that field area to the appropriate crop area sim
} 




#add in sys.time for end point 
save.image(file='myEnvironment_prob_crop.RData')

##scraps/various sections
#field level calculation ----
#let's create some fake data
mat <- as.data.frame(matrix(runif(200,0,1),10))
f<-c(rep(1,50),rep(2,50))
mat <- cbind(f,mat)
mat_n<-as.data.frame(matrix(data=NA,nrow=2,ncol=1000)) #new empty data-frame to store new field values
colnames(mat_n)[1:1000]<-paste0("Sim",1:1000,"")
f<-unique(mat$f)
mat_n<-cbind(f,mat_n)

sum_mat<-mat %>% group_by(f) %>% summarise_all(funs(mean)) 
for (i in 1:nrow(mat_n)){
  for (x in 2:ncol(mat_n)){
    out<-sum_mat[sum_mat$f %in% mat_n[i,1],]
    mat_n[i,x]<-sample(out[,2:30],1)
  }}

field1<-mat_n[2,] #for all simulations in a field, check that the value was sampled from the right averaged row
sum_1<-sum_mat[1,] #pull out individual field values 
field1 %in% sum_1 # ok, looks like we're good, nothing in field sim 2 matches the averages across crops for field 2

#let's create some fake data
mat <- as.data.frame(matrix(runif(3000,0,1),100))
f<-c(rep(1,50),rep(2,50))
mat <- cbind(f,mat)
mat_n<-as.data.frame(matrix(data=NA,nrow=2,ncol=1000)) #new empty data-frame to store new field values
colnames(mat_n)[1:1000]<-paste0("Sim",1:1000,"")
f<-unique(mat$f)
mat_n<-cbind(f,mat_n)

sum_mat<-mat %>% group_by(f) %>% summarise_all(funs(mean)) 
for (i in 1:nrow(mat_n)){
  for (x in 2:ncol(mat_n)){
    out<-sum_mat[sum_mat$f %in% mat_n[i,1],]
    mat_n[i,x]<-sample(out[,2:30],1)
  }}

field1<-mat_n[2,] #for all simulations in a field, check that the value was sampled from the right averaged row
sum_1<-sum_mat[1,] #pull out individual field values 
field1 %in% sum_1 # ok, looks like we're good, nothing in field sim 2 matches the averages across crops for field 2

#new challenge: how do we add in an area constraint for sampling?
#let's make a new data-frame containing the area of each crop per county 
area <- as.data.frame(matrix(runif(60,500,1000),2))
area[3,]<-colSums(area)
area$ID<-c(1,2,'sum')
area<-area[,c(31,1:30)]





#pixel level calculation----
#let's create some fake data
mat <- as.data.frame(matrix(runif(3000,0,1),100))
f<-c(rep(1,50),rep(2,50))
mat <- cbind(f,mat)
mat_n<-as.data.frame(matrix(data=NA,nrow=nrow(mat),ncol=1000)) #new empty data-frame to store new pixel values
colnames(mat_n)[1:1000]<-paste0("Sim",1:1000,"")
mat_n<-cbind(f,mat_n)

sum_mat<-mat %>% group_by(f) %>% summarise_all(funs(mean)) 
for (i in 1:nrow(mat_n)){
  for (x in 2:ncol(mat_n)){
    out<-sum_mat[sum_mat$f %in% mat_n[i,1],]
    mat_n[i,x]<-sample(out[,2:30],1)
  }}

#let's test to see if this worked
field1<-mat_n[51:100,2] 
sum_1<-sum_mat[2,] #pull out individual pixel levels and see if all values match one of the values in the averaged crop probs
field1 %in% sum_1 #if all true, then likely we've nailed it

                  





#scraps----

#let's do a test with some random pixel data and a random set of polygons
r <- raster(nrow=40, ncol=40, xmn=0, xmx=2,ymn=0,ymx=2)
r[] <- round(runif(ncell(r),0,1),0)
plot(r) #this is our field layer

r_prob1 <- setValues(r,runif(ncell(r))) #prob layer
r_prob2 <- setValues(r,runif(ncell(r))) #prob layer
plot(r_prob1) #this is our probability crop layer

x <- sampleRandom(r_prob1, ncell(r_prob1)*.10, asRaster=TRUE)
y <- sampleRandom(r_prob2, ncell(r_prob2)*.10, asRaster=TRUE)
plot(x)
plot(y)



#I think I can use intersect/mask to get the overlapping sample probabilities and calculate area
z <- intersect(r,x)
plot(z)


mat_n[i,x]<- ifelse(sum_mat$f %in% mat_n$f, sample(sum_mat[,2:30]), NA)
out<-sum_mat[sum_mat$f %in% mat_n[1,1], ]



#this is close but not quite there; it doesn't exclusively match the row id to the random sample
for (i in 1:nrow(mat_n)){
  for (x in 2:ncol(mat_n)){
    if(mat_n[i,1] %in% sum_mat$f){
      mat_n[i,x] <- sample(sum_mat[,2:30], 1)
    }
    else mat_n[i,x]<-NA
  }}


#mat_n[i,] <- #where mat_n$id = sum_mat$id, sample mat_n[i,] from sum_mat[,2:30]


#let's create a list again, so that we can loop over IDs
#by_ID<-split(mat, list(mat$f), drop=T) 

#this gets us a closest value, so that's a start
out<-which.min(abs(testy[,2:30]-r1)) #so this says that r1 is closest to almond

rp_r<-as.numeric((var[,r1])) #here is the random value for that field 
(1-(crp_r))/crp_r
crp_r<-names(crp_r)  #here is the name for that field 


#we cna migrate this to the nc? environment
#print (do timestamp)
#do fewer columns? my original dataset of two took a few
#
#this takes at least two hours or more per county (started running at 11:30)

#alternatively - split each field into a list...or split by crop?
#write a function similar to the one below, use lapply
#does Tom have suggestions or similar code? 
#do we add in a threshold for probability?
#1-prob of all crops

#by_ID<-split(mat_n, list(mat$f), drop=T) 
# x is the split empty mat_n
# y is the split sum_mat
# z is the area
pct <- function(x,y,z) {
  for (a in colnames(z)){
    for (i in nrow(x)){
      if ( sum(y[a]) < "some maximum")  sample(x[,2:30],1)
      if ( length(x[x >= p]) == length(x) ) 
        else return( length(x[x >= p]) / length(x) ) 
    }}}

#this returns index number of the corresponding crop
#outside loop over fields
#parallel process - something to speed up the processing
#for area stuff, we'll send it to a different core
#build it out without area so far, and 


#I think what I'll have to do is do a crop-wise list 
#what if we go ahead and sample, but then re-sample or randomly remove/change fields based on area?
#ex: 
#FOR EACH CROP: convert all fields that have a prob above 0 to 1
#calculate total area of these fields by matching them to the IDs in a df containing the total area of each field
#if the total sum area of all our Crop X is > the value for crop X reported in the NASS dataset
#we randomly remove a few fields? change to what?

#challenge: how do we incorporate some sort of spatial constraint by crop for the entire county? 
#do we take the sum of each crop for the whole county? I am stuck on how to get the code to 'approach' a limit

#if sum of crop [a] area in mat_n < sum crop [a] area in area, sample from column crop [a]
#else if sum of crop [a] in mat_n >= sum crop [a], sample from other crop

#other challenge; for a large portion of the fields, many of the crops average zero
#do we skew towards values > 0 for those fields? 



for (i in 1:nrow(mat_n)){
  for (x in 2:ncol(mat_n)){
    out<-mini_dat[mini_dat$ID %in% mat_n[i,1],]
    r1<-sample(out[,2:31],1)
    out<-which.min(abs(testy[,2:31]-r1))
  }
  mat_n[i,x]
}


##this works
#let's try for one row
out<-mini_dat[mini_dat$ID %in% mat_n[1,1],] 
var<-out[,2:31] #pull out crops
probs<-as.numeric(var)
r1<-sample(30, size = 1, replace = TRUE, prob = probs) #use IDs of each crop, and then the probs associated with crop
crp_r<-names(var[,r1]) #ok, excellent; we've assigned a NC probability to that field 

test<- sub("_S.*", "", names(sum_mat[,2:30])) #gonna need to change all names cause they're wonky
colnames(sum_mat)[2:30]<-test
nass_c_f<-nass_c[nass_c$Cover_Type %in% test,] #22, still missing 7
nass_c_f$Cover_Type
#some names may not match, look through crops to see what's missing from sum_mat in nass_c_f, pull out
missing<-c("Almonds", "Dry Beans", "Fallow/Idle Cropland","Honeydew Melons","Misc Vegs & Fruits","Pistachios","Sweet Potatoes")
nass_c_f_m<-nass_c[nass_c$Cover_Type %in% missing,] 
nass_c_f<-rbind(nass_c_f,nass_c_f_m)
