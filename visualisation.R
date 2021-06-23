library(lubridate)
library(dplyr)
library(tibble)
library(SpatialKDE)
library(sp)
library(sf)
library(readr)
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

tmap_mode("view")

tm_shape(wss_buffer3)+
  tm_polygons(col = "red", alpha = 0.3) + 
  tm_shape(wss_buffer2)+
  tm_polygons(col = "blue", alpha = 0.3)+
  tm_shape(wss_buffer1)+
  tm_polygons(col = "green", alpha = 0.3)

#KDE calculation
cell_size <- 200
band_width <- 150

grid_boars <- boar_geom%>%
  create_grid_hexagonal(cell_size = cell_size, side_offset = band_width)

kde_boars <- boar_geom%>%
  kde(band_width = band_width, kernel = "quartic", grid = grid_boars)%>%
  filter(kde_value != 0)

tm_shape(kde_boars)+
  tm_fill(col = "kde_value", palette = "viridis", alpha = 0.3, style = "jenks")+
  tm_borders(col = "NA")+
tm_shape(wss_buffer3)+
  tm_polygons(col = "red", alpha = 0.1) + 
tm_shape(wss_buffer2)+
  tm_polygons(col = "blue", alpha = 0.1)+
tm_shape(wss_buffer1)+
  tm_polygons(col = "green", alpha = 0.1)