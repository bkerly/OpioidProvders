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

OTPLocations <- read_csv("Data/ColoradoOTPGeoTagged.csv", 
                         col_types = cols(X1 = col_skip(),
                                        lat_max = col_skip(), 
                                          lat_min = col_skip(), lon_max = col_skip(), 
                                          lon_min = col_skip()))

DWPLocations <- read_csv("Data/ColoradoDATAProvidersGeoTagged.csv", 
                         col_types = cols(Deg. = col_factor(levels = c("MD", 
                                                                       "DO", "NP", "PA")), X1 = col_skip(), 
                                          lat_max = col_skip(), lat_min = col_skip(), 
                                          lon_max = col_skip(), lon_min = col_skip()))

m <- get_stamenmap(getbb("Colorado"),source="stamen",maptype = "toner",zoom=7)
ggmap(m) +
  geom_point(data=DWPLocations, aes(x=lon, y=lat, alpha = 1, color = "Green",size=3),position="jitter") +
geom_point(data=OTPLocations, aes(x=lon, y=lat, alpha = 1, color = "Black"),size=3) +
  scale_alpha(guide=FALSE)+
  scale_size(guide=FALSE) +
  labs(color = "MAT Type") +
  scale_color_discrete(labels = c("OTP", "DWP"))
  
