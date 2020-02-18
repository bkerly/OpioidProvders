library(spData)
library(lwgeom)
library(tidycensus)
library(sf)
library(dplyr)

#OK so this first part is all just fluff to create fake
#OD locations.  I am sure there is a better way, but
#just deal with it

census_api_key("efb75012a4507416a09b0fbd0c90b9b2b4582632")
co17 <- get_acs(geography = "tract", 
                variables = c(totpop="B03002_001"), 
                state = "CO", year=2017, geometry = TRUE)
co18<-(co17[,6])
co18$centroids <- st_transform(co18, 29101) %>% 
                  st_centroid() %>% st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
                  st_geometry()             


  #Pick random census tract for OD locations for now
p<-co18[c(5,18,343),]



#Now that we have some OD locations (p) the code actually begins

  #First we'll find the closest service location to each OD

  #Convert to Geographic Object

docs<-read.csv("/Users/ktsabin/Desktop/Brian Stuff/SalishRG/Opioid Provider Mapping/OpioidProvders/ColoradoOTPGeoTagged.csv")
docs2<-docs[,c(2:3,11:12)]%>%distinct()
docs3<-docs2[complete.cases(docs2),]
docs4 <- st_as_sf(docs3, coords = c("lon", "lat"), crs = 4326)
docs4$centroids <- st_transform(docs4, 29101) %>% 
  st_centroid() %>% st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%  st_geometry()      

  #Distance from Each OD to Nearest service location

docs5<-as.data.frame(st_distance(p$centroids,docs4$centroids))

  #Minimum Distance to service location from each OD

docs6<-docs5%>%mutate(y=apply(docs5, 1, FUN=min))%>%select(y)


#Now we will find the distance from each OD to each census tract and identify which
#census tract will shrink the total distance the most

  #1. Getting Census Tract and Finding Coordinates of Centroid
  #2. Calculating distance from each census tract to each OD locations
  #3. Fugly Loop: for each census tract find the closest service location to each OD if the CT had a service location 
  #4. Finding the census tract that would minimize the sum distance for all the OD

  #Getting Geography
census_api_key("efb75012a4507416a09b0fbd0c90b9b2b4582632")
co17 <- get_acs(geography = "tract", 
                variables = c(totpop="B03002_001"), 
                state = "CO", year=2017, geometry = TRUE)


  #Get centroid of each census tract
co18<-(co17[,6])
co18$centroids <- st_transform(co18, 29101) %>% 
  st_centroid() %>% st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
  st_geometry()  


  #Some loops (because I am bad at R) to determine the shortest distanct
  #to a service location from each OD.  Each loop pretends like our new
  #location is in a different census tract

q<-as.data.frame(st_distance(co18$centroids,p$centroids))
ND<-1

for (l1 in 1:nrow(q)) {
  
   
    for (l2 in 1:ncol(q)){
      q[l1,l2]<-min(as.numeric(q[l1,l2]),docs6[l2,])
      
    }
  
  ND[l1]<-sum(t(q[l1,]),rm.na=TRUE)
  
}


#Comparing Distances with and without a new location
q$Original_Distance<-sum(docs6$y,rm.na=TRUE)
q$New_Distance<-ND
q$Time_Saved<-(q$Original_Distance-q$New_Distance)/ncol(q)

#Picking the location with the minmum sum distance.  I guess we could do median too
#if we preferred
out<-q%>% cbind(as.data.frame(co17[,c(1:2,4)]))%>%
    filter(New_Distance==min(New_Distance))



paste("Best Census Tract for New Location:", out$GEOID,
      "Average Distance Saved:", round(out$Time_Saved),"m.........Wow!")
  
#Expected result: "Best Census Tract for New Location: 08015000200 Average Distance Saved: 23385 m.........Wow!"

                