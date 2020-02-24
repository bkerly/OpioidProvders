#Clean the canvas
remove(list = ls())

#Set WD to the location of the script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the relevant libraries - do this every time
library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(ggrepel)
library(tidyverse)
library(ggmap)
library(osmdata)
library(sf)
library(spData)
library(lwgeom)
library(tidycensus)
library(RColorBrewer)
library(osrm)
library(geosphere)


###Get centroid of all the census tracts----
census_api_key("efb75012a4507416a09b0fbd0c90b9b2b4582632")

co17 <- get_acs(geography = "tract", 
                variables = c(totpop="B03002_001"), 
                state = "CO", year=2017, geometry = TRUE)
#Get centroid of each census tract
co18<-(co17[,6])
co18$centroids <- st_transform(co18, 29101) %>% 
  st_centroid() %>% st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
  st_geometry() 
CTCoords <- do.call(rbind, st_geometry(co18$centroids)) %>%
  as.data.frame() %>%
  `colnames<-`(c("lon","lat"))
CTCoords <- bind_cols(co17,CTCoords,id=NULL) %>%
  select("GEOID","lat","lon")

###Load the data and prepreocess it a bit----
#Load in death data by census tract
deaths <- read_csv("Data/co_opioid_death_1418.csv")
deaths$d <- ifelse(deaths$d == ".",1,deaths$d)
deaths$d <- as.numeric(deaths$d)

#Bind with census tract centroid location to generate approximate lat/long
deaths <- left_join(deaths,CTCoords,by=c("tractid"="GEOID")) %>%
  as.data.frame() %>%
  select("tractid","d","lat","lon") %>%
  filter(nchar(tractid)>10)

#Add in weightings for deaths (so that if there are two deaths, now there are two rows)
deaths2 <- with(deaths, deaths[rep(1:nrow(deaths), d),]) %>%
  select(-d)

#Load data about the location of DATA providers
DATAproviders <- read_csv("Data/ColoradoDATAProvidersGeoTagged.csv")
  
#Load data about the location of OTP
OTPs <- read_csv("Data/ColoradoOTPGeoTagged.csv")
colnames(OTPs)[2] <- "ProgramName"

###Prepare for mapping---
#Define some colors
DATA_Provider_Locations = "#011f4b"

OTP_Locations = "#6497b1"

Death_Locations = "#b3cde0"

col4 = "#CC0000"

#Google map Api
ggmap::register_google(key = "AIzaSyD5w9yNOufvfRxcdGX0TlTZV6nIU63vTEk")

###Determining travel times---
#Preparing the death dataframe
DeathsSRC <- deaths2 %>%
  select(tractid,lon,lat)
#Preparing the OTP dataframe
OTPDst <- OTPs %>%
  mutate(OTPName = paste(ProgramName,DBA,sep="")) %>%
  select(OTPName,lon,lat) %>%
  as.data.frame()

#Calculating the linear distance between each death and OTP----
#distm(c(DeathsSRC[1,2],DeathsSRC[1,3]),c(OTPDst[1,2],OTPDst[1,3]),fun=distHaversine)

#Set up the length of the progress bar
n<-nrow(DeathsSRC)
#Loop through each death and each OTP to find the linear distance betwewen them
##Create and empty destination dataframe
LinDistDeathOTP <- data.frame(
  DeathTractID = character(),
  OTPName = character(),
  Distance = numeric(),
  stringsAsFactors = FALSE
)

for(i in 1:nrow(DeathsSRC)){
  #Create a new empty data frame for each death location
  df <- data.frame(
    DeathTractID = character(),
    OTPName = character(),
    Distance = numeric(),
    stringsAsFactors = FALSE
  )
  
  #Fill it up with all the OTP locations for that single location
  for(j in 1:nrow(OTPDst)){
    df[j,1] <- as.character(DeathsSRC[i,1])
    df[j,2] <- as.character(OTPDst[j,1])
    df[j,3] <- 
      distm(c(DeathsSRC[i,2],DeathsSRC[i,3]),c(OTPDst[j,2],OTPDst[j,3]),fun=distHaversine)
  
    
    }
  #And then add it to the results dataframe
  LinDistDeathOTP <- bind_rows(LinDistDeathOTP,df)
  #Flickery progress bar!
  cat(paste0(round(i / n * 100), '% completed'))
  width <- options()$width
  cat(paste0(rep('=', i / n * width), collapse = ''))
  Sys.sleep(.005)
  if (i == n) cat('\014Done')
  else cat('\014')

}

#For each Death, pick the nearest 5 OTP
NearestOTPs <- LinDistDeathOTP %>%
  arrange((Distance)) %>%
  group_by(DeathTractID) %>%
  slice(1:5) %>%
  as.data.frame()

#Rejoin in the lon and lat of the deaths and OTPs
NearestOTPs <- left_join(NearestOTPs,DeathsSRC,
                         by=c("DeathTractID" = "tractid")) %>%
  left_join(OTPDst,by="OTPName") %>%
  `colnames<-`(c("DeathTractID","OTPName","Distance","DeathLon","DeathLat","OTPLon","OTPLat"))

#Making a little mapsky of lines between deaths and OTPs
m <- get_stamenmap(getbb("Colorado"),source="stamen",maptype = "toner",zoom=7)
ggmap(m) +
  geom_segment(data=NearestOTPs, aes(x=DeathLon,y=DeathLat,xend=OTPLon,yend=OTPLat,color="black"),alpha=1/10) +
    geom_point(data = NearestOTPs, aes(x=DeathLon,y=DeathLat,color="red"),shape=1) +
  geom_point(data = NearestOTPs, aes(x=OTPLon,y=OTPLat,color="cyan"),shape=3)
  
#Finding the travel time between each death and the nearest 5 OTPs (but only the first 100 rows)
DeathsToOTPTime <- osrmTable(src = select(head(NearestOTPs,100),'DeathTractID','DeathLon','DeathLat'),
                             dst = select(head(NearestOTPs,100),'OTPName','OTPLon','OTPLat'))
durations <- DeathsToOTPTime[["durations"]]
sources<- DeathsToOTPTime[["sources"]]
destinations <- DeathsToOTPTime[["destinations"]]
#This output is weird and I'm still figuring out how to work with it

#Pick the fastest travel time
DeathsToNearestOTPTime <- DeathsToOTPTime %>%
  arrange(Durations) %>%
  group_by(DeathTractID) %>%
  slice(1) %>%
  ungroup()

#Rejoin in the lon and lat of the deaths and OTPs
NearestOTPs <- left_join(NearestOTPs,DeathsSRC,
                         by=c("DeathTractID" = "tractid")) %>%
  left_join(OTPDst,by="OTPName") %>%
  `colnames<-`(c("DeathTractID","OTPName","Distance","DeathLon","DeathLat","OTPLon","OTPLat"))

###Make a cool map of travel routes---
#Rejoin in the lon and lat of the deaths and OTPs
DeathsToNearestOTPTime <- left_join(DeathsToNearestOTPTime,DeathsSRC,
                         by=c("DDeathTractID" = "tractid")) %>%
  left_join(OTPName,by="OTPName") %>%
  `colnames<-`(DeathTractID,OTPName,Distance,DeathLon,DeathLat,OTPLon,OTPLat)

#Ask OSRM to find the routes
DeathsToNearestOTPRoutes <- osrmTable(src = DeathsToNearestOTPTime[['DeathTractID','DeathLon','DeathLat']],
                                      dst = DeathsToNearestOTPTime[['OTPName','OTPLon','OTPLat']],
                                      returnclass = "sf")
ggplot()+
  geom_sf(data=DeathsToNearestOTPRoutes)
plot(st_geometry(DeathsToNearestOTPRoutes), col = "red", lwd = 2)


m <- get_stamenmap(getbb("Colorado"),source="stamen",maptype = "toner",zoom=7)
ggmap(m) +
  stat_density2d(data=deaths2, aes(x=lon, y=lat, fill = ..level.., alpha = ..level..), bins=10, geom = 'polygon')+
  scale_fill_gradient(low = "blue", high = "red") +
  scale_alpha(range = c(0.05, 0.8), guide = FALSE) +
  geom_point(data=deaths2, aes(x=lon, y=lat, alpha = 1, color = "Death_Locations"),position="jitter")+
  geom_point(data=DATAproviders, aes(x=lon, y=lat, alpha = 1, color = "DATA_Provider_Locations"))+
  geom_point(data=OTPs, aes(x = lon, y= lat, alpha = 1, color= "OTP_Locations"))

