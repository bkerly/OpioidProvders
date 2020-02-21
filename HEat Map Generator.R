rm(ls(all.names = TRUE))

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

#Get centroid of all the census tracts
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
  `colnames<-`(c("lat","lon"))
CTCoords <- bind_cols(co17,CTCoords,id=NULL) %>%
  select("GEOID","lat","lon")

deaths <- read_csv("/Users/brianerly/BEEFERS/OpioidProvders/Data/co_opioid_death_1418.csv")
deaths$d <- ifelse(deaths$d == ".",1,deaths$d)
deaths$d <- as.numeric(deaths$d)
deaths <- left_join(deaths,CTCoords,by=c("tractid"="GEOID")) %>%
  as.data.frame() %>%
  select("tractid","d","lat","lon")


DATAproviders <- read_csv("Data/ColoradoDATAProvidersGeoTagged.csv")
  
OTPs <- read_csv("Data/ColoradoOTPGeoTagged.csv")

DATA_Provider_Locations = "#011f4b"

OTP_Locations = "#6497b1"

Death_Locations = "#b3cde0"

col4 = "#CC0000"

ggmap::register_google(key = "AIzaSyD5w9yNOufvfRxcdGX0TlTZV6nIU63vTEk")

m <- get_map(getbb("Colorado"),source="stamen",maptype = "toner-background")
ggmap(m) + 
  geom_point(data=deaths, aes(x=lon, y=lat, color = "Death_Locations"))+
  geom_point(data=DATAproviders, aes(x=lon, y=lat, color = "DATA_Provider_Locations"))+
  geom_point(data=OTPs, aes(x = lon, y= lat, color= "OTP_Locations"))

