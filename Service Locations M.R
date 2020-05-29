library(tidycensus)
library(sf)
library(dplyr)
library(osrm)
library(usmap)
library(ggplot2)


#Getting Geography (Centroid) of all Census Tracts
census_api_key("efb75012a4507416a09b0fbd0c90b9b2b4582632")
geo1 <- get_acs(geography = "tract", 
                variables = c(totpop="B03002_001"), 
                state = "CO", year=2017, geometry = TRUE)

geo2<-(geo1[,c(1,6)])
geo2$centroids <- st_transform(geo2$geometry, 29101) %>% 
  st_centroid() %>% st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
  st_geometry()  

geo3<-as.data.frame(geo2[,3])[,1:2]
geo4<-data.frame(matrix(unlist(geo3[1]), nrow=nrow(geo3), byrow=T))%>%
  cbind(data.frame(matrix(unlist(as.data.frame(geo2[,1])[,1:2][1]), nrow=nrow(geo2), byrow=T)))%>%
  rename(id=3)

#Getting Death Data, Formatting, Geocoding

dths<-read.csv("C:/Users/sje0303/Downloads/co_opioid_death_1418.csv")
dths$GEOID<-paste("0",as.character(dths$GEOID),sep="")
dths2<-geo2%>%inner_join(dths)%>%filter(d>0)
dthloc<-as.data.frame(dths2[,6])[,1:2]
dthloc2<-data.frame(matrix(unlist(dthloc[1]), nrow=nrow(dthloc), byrow=T))
dthloc2$id<-dths2$GEOID

deathcount<-nrow(dths2) #Indicator for Number of Records


#Getting Service Locations, Formatting, Geocoding

docs<-read.csv("C:/Users/sje0303/Downloads/ColoradoDATAProvidersGeoTagged.csv")
docs2<-docs[,c(3:4,6,14:15)]%>%distinct()
docs3<-docs2[complete.cases(docs2),]
docs4 <- st_as_sf(docs3, coords = c("lon", "lat"), crs = 4326)
docs4$centroids <- st_transform(docs4, 29101) %>% 
  st_centroid() %>% st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%  st_geometry()      
doctor<-as.data.frame(docs4[,5])[,1:2]
doctor1<-data.frame(matrix(unlist(doctor[1]), nrow=nrow(doctor), byrow=T))
doctor1$id<-paste(docs3$Last,docs3$First,docs3$Address)


doccount<-nrow(docs3) #Indicator for Number of Records



#First we'll find the closest service location to each OD as the crow flies

dist<-as.data.frame(st_distance(docs4$centroids,dths2$centroids))


#Now Calculating Drive times.  The API has a cap on queries, so running one OD at a time 
#Only calculating drive time for the 99 shortest distances

#This is a frame of all the service location to merge drive time onto as the program loops.
frame <- doctor1

for (i in c(1:nrow(dthloc))){
  
  
  id<-paste("D",i,sep="")
  
  #Taking First 99 Closest Census Tracts to Each OD Location
  dist2<-dist%>%as.matrix%>%as.data.frame()%>%cbind(doctor1)%>%rename(hold := i)%>%
    arrange(hold)%>%head(99)%>%select(X1,X2,id)
  
  #Stacking 1 OD location onto list to calculate distance to each OD Location
  dist3<-rbind(dthloc2[i,],dist2)%>%rename( lon=X1, lat=X2) %>%select(id, lon,lat)%>%
    mutate(lon=lon, lat=lat)
  
  #Drive Time Table
  dist4<-osrmTable(dist3, src = NULL, dst = NULL, exclude = NULL,
                   gepaf = FALSE, measure = "duration")
  
  #List of Drive times from 1 OD location to the closest 99 census tracts
  dist5<-as.data.frame(unlist(dist4[1])[2:100])%>%rename(dist=1)
  dist6<-dist2[2:100,]
  dist6$dist<-dist5
  
  dist5b<-dist4[["durations"]][2:100,1]
  dist5a<-dist4[["durations"]][1,2:100]
  
  dist8<-as.data.frame(cbind(dist5a,dist5b))%>% rowwise()%>%mutate(time=(min(dist5a,dist5b)))%>%select(time)%>%
    as.data.frame()
  dist8$id<-dist2[2:100,3]
  
  frame<-frame%>%left_join(dist8,id=id)%>%mutate(time=ifelse(is.na(time)==0,time,99999))%>%rename(!!id:=time)
  print(i)
}  


#Minimum Distance to service location from each OD
frame2<-frame[,4:387]
frame3<-as.data.frame(apply(frame2, 2, FUN=min))
frame3$weight<-dths2$d  #This Variable is a Weight for the number of OD at a given location

frameout<-cbind(dthloc2,frame3)
write.csv(frameout, file="//doh/user/fr/sje0303/Documents/service2.csv")



#Next up, finding the distance/driving time from each census tract (where we might put a new clinic)
#to each OD via the same algorithm


#Straight Line Distance
q<-as.data.frame(st_distance(geo2$centroids,dths2$centroids))



#Frame for merging loops onto
frame_a<-geo4


#Using OSRM calls for 100 closest distances as before

for (i in 1:nrow(dthloc)){
  
  
  id<-paste("D",i,sep="")
  
  #Taking First 99 Closest Census Tracts to Each OD Location
  q2<-q%>%as.matrix%>%as.data.frame()%>%cbind(geo4)%>%rename(hold := i)%>%
    arrange(hold)%>%head(99)%>%select(X1,X2,id)
  
  #Stacking 1 OD location onto list to calculate distance to each OD Location
  q3<-rbind(dthloc2[i,],q2)%>%rename( lon=X1, lat=X2) %>%select(id, lon,lat)%>%
    mutate(lon=lon, lat=lat)
  
  #Drive Time Table
  q5<-osrmTable(q3, src = NULL, dst = NULL, exclude = NULL,
                gepaf = FALSE, measure = "duration")
  
  #List of Drive times from 1 OD location to the closest 99 census tracts
  q6<-as.data.frame(unlist(q5[1])[2:100])%>%rename(dist=1)
  q7<-q3[2:100,]
  q7$dist<-q6
  
  q6b<-q5[["durations"]][2:100,1]
  q6a<-q5[["durations"]][1,2:100]
  
  q7<-as.data.frame(cbind(q6a,q6b))%>% rowwise()%>%mutate(time=(min(q6a,q6a)))%>%select(time)%>%
    as.data.frame()
  q7$id<-q3[2:100,1]
  
  frame_a<-frame_a%>%left_join(q7,id=id)%>%mutate(time=ifelse(is.na(time)==0,time,99999))%>%rename(!!id:=time)
  
}  



#Loop to look compare current distances from OD to provider (frame3)
#To what it would be with a new provider at each census tract (frame_a)


#Setting up some datasets for looping
out<-as.data.frame(t(frame3[1]))     #Current Minimum Distances for each OD
result<-data.frame("Baseline",sum(out<60)/nrow(frame3),stringsAsFactors = F)  #Frame for Results to stack onto



for (k in 1:3) {
  
  ND<-1                              #Placeholder
  Ncover<-1
  minim<-frame_a[,4:ncol(frame_a)]   #Census Tract-OD Distance Matrix (To Fill in with Loops)
  newframe<-out[1,1:deathcount]      #Current minimum distances for each OD
  
  #Loop through each row of frame_a (each census tract):
  
  for (l1 in 1:nrow(minim)) {
    
    #Taking minimum distance from OD to provider, whether it be an existing provider (newframe)
    # or the new census tract (minim):
    
    for (l2 in 1:ncol(minim)){
      minim[l1,l2]<-min(as.numeric(minim[l1,l2]),as.numeric(newframe[l2]))
      
    }
    
    #Calculating sum distance for each census tract evaluated multiplied by the number of cases in each location
    ND[l1]<-sum(t(minim[l1,]),rm.na=TRUE)
    Ncover[l1]<-sum(minim[l1,1:deathcount]<60)
  }
  
  
  minim$Original_Distance<-sum(frame3[,1],rm.na=TRUE)  #Original distance
  minim$New_Distance<-ND                                           #Distance with new clinic at census tract X
  minim$Time_Saved<-(minim$Original_Distance-minim$New_Distance)/ncol(minim)
  minim$Ncover<-Ncover/deathcount
  #Picking the location with the minmum sum distance.
  
  prints<-frame_a[,3]%>% cbind(minim[,c("Time_Saved", "Ncover", "New_Distance")])%>%as.data.frame()%>%arrange(New_Distance)%>%
    rename(GEOID=1)%>% filter(Ncover==max(Ncover))
  
  print(paste("Best Census Tract for New Location:", head(prints$GEOID,1),
              "Average Time Saved:", head(prints$Time_Saved,1),"minutes",
              "(", head(prints$Ncover,1),"% Covered)"))
  
  
  
  #Updated Min Distance Table for Next Loop!
  out<-minim %>% filter(Ncover==max(Ncover))%>% arrange(New_Distance)
  out<-out[1,1:deathcount]
  
  
  result[k+1,1]<-as.character(head(prints$GEOID,1))
  result[k+1,2]<-sum(out[1,1:deathcount]<60)/nrow(frame3)
  result[k+1,3]<-head(prints$Time_Saved,1)
  
}





target<-as.list(result[-1,1])


data<-docs3[which(docs3$lon>(-115)& docs3$lon<(-101)),c(5,4)]
transformed_data <- usmap_transform(data)


data<-as.data.frame(dths2[,6])[1]
data2<-data.frame(matrix(unlist(data), nrow=nrow(data), byrow=T))%>%rename(lat=X1, lon=X2)
transformed_data2 <- usmap_transform(data2)%>% cbind(dths2$d)

outpl<-minim%>% cbind(as.data.frame(geo4[,c(1:3)]))%>%
  filter(id %in% target)
data<-as.data.frame(outpl[,389:390])
transformed_data3 <- usmap_transform(data)


plot_usmap("counties",include = c("CO")) +
  labs(title = "Doctor Locations in CO" )+
  geom_point(data = transformed_data, 
             aes(x = lon.1, y = lat.1), 
             color = "blue",
             size = 3)



plot_usmap("counties",include = c("CO")) +
  labs(title = "Best New Locations" )+
  geom_point(data = transformed_data2, 
             aes(x = lat.1, y = lon.1), 
             color = "red",
             size =transformed_data2$d)+
  geom_point(data = transformed_data, 
             aes(x = lon.1, y = lat.1), 
             color = "blue",
             size = 3)+
  geom_point(data = transformed_data3, 
             aes(x = X1.1, y = X2.1), 
             color = "yellow",
             size = 3)


