#Probably dont need all these librarys

library(tidycensus)
library(dplyr)
library(sf)
require(reshape2)
library(raster)
library(dplyr)
library(spData)
library(lwgeom)


#Get Census Data really just for geography

census_api_key("efb75012a4507416a09b0fbd0c90b9b2b4582632")
co17 <- get_acs(geography = "tract", 
                variables = c(totpop="B03002_001"), 
                state = "CO", year=2017, geometry = TRUE)


#Get centroid of each census tract
co18<-(co17[,6])
co18$centroids <- st_transform(co18, 29101) %>% 
                  st_centroid() %>% 
                  # this is the crs from d, which has no EPSG code:
                  st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
                  # since you want the centroids in a second geometry col:
                  st_geometry()             


#Pick random points for overdose locations
p<-co18[c(5,18,343),]

#Get distance from all census tracts to overdose locations
q<-as.data.frame(st_distance(co18$centroids,p$centroids))


#Now lets do some picking!

#Part 1: Ideal Place for 1 SSP (closest total distance)
q2<-q%>%filter(V1+V2+V3==min(V1+V2+V3))


#Part2: What if there is existing facility at say row 448, where should we put the next one?

ex<-q[448,]
q2<-q%>%rowwise()%>%mutate(y=min(V1,ex[,1])+min(V2,ex[,2])+min(V3,ex[,3]))%>%
  ungroup()%>%filter(y==min(y))
  
  