library(lubridate)
library(dplyr)
library(tibble)
library(sf)
library(tmap)

wss_data <- read_delim("data/range_class_data.csv", delim = ",")
boar_data <- read_delim("data/boar_locations_filtered.csv", delim = ",")
    
wss_geom <- wss_data%>%
  st_as_sf(coords = c("E", "N"), crs = 2056)

boar_geom <- boar_data%>%
  st_as_sf(coords = c("E", "N"), crs = 2056)

#calculating buffers
wss_buffer1 <- st_buffer(wss_geom, dist = 50)
wss_buffer2 <- st_buffer(wss_geom, dist = 150)
wss_buffer3 <- st_buffer(wss_geom, dist = 300)

#bounding box for KDE
boar_bb <- st_as_sfc(st_bbox(boar_geom))
