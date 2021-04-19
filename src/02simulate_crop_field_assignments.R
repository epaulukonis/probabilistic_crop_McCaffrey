print("stepping into 02simulate_crop_field_assignments.R")
# crop_raster_stack was crop_stackf

##Step 2: Simulate field level probabilities
#extract to fields
datff<- extract(crop_raster_stack, mercedf, df=T) #this extracts the raster cell values by polygons to a df
sum_mat<-datff %>% group_by(ID) %>% summarise_all(funs(mean)) #summarize each crop by field to mean
sum_c <- apply(sum_mat[,c(2:30)], 1, sum) 
sum_mat$NC<-round(1-sum_c,4) #add in column for non-crop
sum_mat<-na.omit(sum_mat) #omit fields which don't overlap with crop data
mat_n<-as.data.frame(matrix(data=NA,nrow=nrow(sum_mat),ncol=1000)) #set up empty df to hold simulations
colnames(mat_n)[1:1000]<-paste0("Sim",1:1000,"")
ID<-unique(sum_mat$ID)
mat_n<-cbind(ID,mat_n)

#function to convert the rasters to binary
fun_c <- function(x) {
  x[x>0] <- 1
  return(x)
}
out<-stack(calc(crop_raster_stack, fun_c)) #put that in a raster stack

print("calculating raster areas")
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

##loaded environment contains everything above up to here
#see how long this takes
sleep_for_a_minute <- function() { Sys.sleep(60) }
start_time <- Sys.time()
sleep_for_a_minute()

"big loop over simulations and fields"
for (j in 2:ncol(mat_n)){ #1000
  for (i in 1:nrow(mat_n)){ #16000
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
  print(paste("finished", j,"out of 1000 simulations"))
}

end_time <- Sys.time()
end_time - start_time

#add in verification after running

print("finished!")

#save.image(file='myEnvironment_prob_crop.RData') 
