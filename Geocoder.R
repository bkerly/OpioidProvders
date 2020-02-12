#Sets up the environment
rm(list=ls())
library(readr)
library(jsonlite)
library(tidyverse)
library(tmaptools)
library(XML)
library(ggmap)

#Reads in the soure data for OTPs
ColoradoOTP <- read_csv("Downloads/ColoradoOTP.csv")

#Trims out irrelevant information, like suite and apartment numebr
ColoradoOTP$Street <- ColoradoOTP$Street %>%
  gsub("Suite.*","",.) %>%
  gsub("Ste.*","",.) %>%
  gsub("Unit.*","",.) %>%
  gsub("#.*","",.) %>%
  gsub("Mailstop.*","",.) %>%
  gsub("Mail Stop.*","",.) %>%
  gsub("Apt.*","",.) %>%
  gsub("Apartment.*","",.) %>%
  gsub(",.*","",.)

#Puts all the address query information into one column
ColoradoOTP$AddressFull <- paste(ColoradoOTP$Street,
                                 ColoradoOTP$City,ColoradoOTP$State,
                                 ColoradoOTP$Zipcode,sep = ", ")


#Runs the query for OTP
ColoradoOTP <- geocode_OSM(ColoradoOTP$AddressFull) %>%
  rename(AddressFull = query) %>%
  left_join(ColoradoOTP,., by = "AddressFull")

#Reads in the source data for DATA providers
ColoradoDATA <- read_csv("Downloads/ColoradoDATAJoined.csv")

#Trims out irrelevant information, like suite and apartment numebr
ColoradoDATA$Address <- ColoradoDATA$Address %>%
  gsub("Suite.*","",.) %>%
  gsub("Ste.*","",.) %>%
  gsub("Unit.*","",.) %>%
  gsub("#.*","",.) %>%
  gsub("Mailstop.*","",.) %>%
  gsub("Mail Stop.*","",.) %>%
  gsub("Apt.*","",.) %>%
  gsub("Apartment.*","",.) %>%
  gsub(",.*","",.)

#Puts all the address query information into one column
ColoradoDATA$AddressFull <- paste(ColoradoDATA$Address,
                                  ColoradoDATA$City,
                                  ColoradoDATA$State,
                                  ColoradoDATA$`Postal Code`)

#Adds a 1 second pauase between queries of teh geocode_OSM() funciton
GeocodeWithPause <- function (q, projection = NULL, return.first.only = TRUE, details = FALSE, 
          as.data.frame = NA, as.sf = FALSE, server = "http://nominatim.openstreetmap.org") 
{
  n <- length(q)
  q2 <- gsub(" ", "+", enc2utf8(q), fixed = TRUE)
  addr <- paste0(server, "/search?q=", q2, "&format=xml&polygon=0&addressdetails=0")
  project <- !missing(projection)
  if (project) 
    projection <- get_proj4(projection, output = "crs")
  if (is.na(as.data.frame)) 
    as.data.frame <- (n > 1)
  if (as.sf) {
    as.data.frame <- TRUE
    return.first.only <- TRUE
  }
  output2 <- lapply(1:n, function(k) {
    tmpfile <- tempfile()
    suppressWarnings(download.file(addr[k], destfile = tmpfile, 
                                   mode = "wb", quiet = TRUE))
    doc <- xmlTreeParse(tmpfile, encoding = "UTF-8")
    unlink(tmpfile)
    res <- xmlChildren(xmlRoot(doc))
    if (length(res) == 0) {
      warning(paste("No results found for \"", q[k], "\".", 
                    sep = ""))
      return(NULL)
    }
    idx <- if (return.first.only) 
      1
    else 1:length(res)
    sn_names <- c("place_id", "osm_type", "osm_id", "place_rank", 
                  "display_name", "class", "type", "importance", "icon")
    output <- lapply(idx, function(i) {
      Sys.sleep(1)
      search_result <- xmlAttrs(res[[i]])
      search_result_id <- search_result[sn_names]
      names(search_result_id) <- sn_names
      Encoding(search_result_id) <- "UTF-8"
      search_result_loc <- as.numeric(search_result[c("lat", 
                                                      "lon")])
      names(search_result_loc) <- c("lat", "lon")
      search_result_bb <- as.numeric(unlist(strsplit(search_result["boundingbox"], 
                                                     ",")))
      if (!project) {
        names(search_result_bb) <- c("lat_min", "lat_max", 
                                     "lon_min", "lon_max")
        b <- bb(xlim = search_result_bb[3:4], ylim = search_result_bb[1:2])
        coords <- search_result_loc[c("lon", "lat")]
        names(coords) <- c("x", "y")
      }
      else {
        b <- bb(xlim = search_result_bb[3:4], ylim = search_result_bb[1:2], 
                current.projection = .CRS_longlat, projection = projection)
        search_result_bb <- b[c(2, 4, 1, 3)]
        names(search_result_bb) <- c("y_min", "y_max", 
                                     "x_min", "x_max")
        p <- st_sf(st_sfc(st_point(search_result_loc[2:1]), 
                          crs = .crs_longlat))
        p <- set_projection(p, projection = projection)
        coords <- as.vector(st_coordinates(p))
        names(coords) <- c("x", "y")
        search_result_loc <- as.list(coords)
        names(search_result_loc) <- c("x", "y")
      }
      res <- if (as.data.frame) {
        c(list(query = q[k]), search_result_loc, search_result_bb)
      }
      else {
        c(list(query = q[k], coords = coords, bbox = b))
      }
      if (details) 
        res <- c(res, search_result_id)
      if (as.data.frame) 
        res <- as.data.frame(res, stringsAsFactors = FALSE)
      res
    })
  })
  output3 <- do.call(c, output2)
  if (as.data.frame) {
    df <- do.call(rbind, output3)
    if (as.sf) {
      if (!project) {
        df$x <- df$lon
        df$y <- df$lat
        res <- st_as_sf(df, coords = c("x", "y"), crs = .crs_longlat)
      }
      else {
        df$x2 <- df$x
        df$y2 <- df$y
        res <- st_as_sf(df, coords = c("x2", "y2"), crs = .crs_longlat)
      }
      res
    }
    else {
      df
    }
  }
  else {
    if (length(output3) == 1) {
      output3[[1]]
    }
    else output3
  }
}

#Runs the query for DATA providers
ColoradoDATA <- GeocodeWithPause(ColoradoDATA$AddressFull) %>%
  rename(AddressFull = query) %>%
  left_join(ColoradoDATA,., by = "AddressFull")



# ColoradoDATA <- ColoradoDATA %>%
#   select(-lat.y,-lon.y,lat_min.y,lat_max.y,lon_min.y,lon_max.y)%>%
#   rename(lat=lat.x,
#          lon=lon.x,
#          lat_min=lat_min.x,
#          lat_max=lat_max.x,
#          lon_min=lon_min.x,
#          lon_max=lon_max.x)

#Runs the query using Google's API for where OSM fails
df <- ColoradoDATA %>%
  filter(is.na(lat))
df2 <- geocode(df$AddressFull)
df3 <- bind_cols(df,df2) %>%
  select(-lat,-lon)%>%
  rename(lon = lon1, lat = lat1)
ColoradoDATAGeoTagged <- ColoradoDATA %>%
  filter(!is.na(lat)) %>%
  bind_rows(df3) %>%
  select(-lat_min.y,-lat_max.y,
         -lon_min.y,-lon_max.y)

write.csv(ColoradoOTP,"Downloads/ColoradoOTPGeoTagged.csv")
write.csv(ColoradoDATAGeoTagged,"Downloads/ColoradoDATAProvidersGeoTagged.csv")
