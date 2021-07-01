library(lubridate)
library(dplyr)
library(tibble)
library(SpatialKDE)
library(sp)
library(sf)
library(readr)
library(tmap)
library(shiny)
library(RColorBrewer)

wss_data <- read_delim("data/range_class_data.csv", delim = ",")
boar_data <- read_delim("data/boar_locations_filtered.csv", delim = ",")

wss_data <- wss_data%>%
  mutate(id = paste(as.character(X1), ":", " ", flurname, sep = ""))
  
#functions
hex_grids <- function(geom, extent, cell_size){
  
  grid <- extent%>%
    create_grid_hexagonal(cell_size = cell_size)%>%
    st_sf()%>%
    mutate(id_hex = 1:n())%>%
    dplyr::select(id_hex, geometry)
  
  grid_geom_joined <- st_join(geom, grid)
  grid_count <- grid_geom_joined%>%
    st_drop_geometry()%>%
    count(id_hex)
  
  hex_grid_geom <- grid%>%
    left_join(grid_count)#%>%
    #filter(!is.na(n))
  
  hex_grid_geom[is.na(hex_grid_geom)] <- 0
  
  return(hex_grid_geom)
}

#converting to geometry
wss_geom <- wss_data%>%
  st_as_sf(coords = c("E", "N"), crs = 2056)%>%
  filter(X1 == 4)

#Setting parameters
cell_size <- 100 #cell size in meters for the grid binning
wss_buffer1 <- st_buffer(wss_geom, dist = 50)
wss_buffer2 <- st_buffer(wss_geom, dist = 150)
wss_buffer3 <- st_buffer(wss_geom, dist = 300)

donut_2 <- st_difference(wss_buffer2, wss_buffer1)
donut_3 <- st_difference(wss_buffer3, wss_buffer2)

start_date <- wss_geom$datum_on
end_date <- wss_geom$datum_off
n_seconds <- as.numeric(difftime(end_date, start_date, units = "secs"))

 
#honeycomb grid binning on and before on
boar_geom_on <- st_as_sf(boar_data, coords = c("E", "N"), crs = 2056)
boar_bbox <- st_as_sf(st_make_grid(boar_geom_on, n = 1))
boar_geom_on <- filter(boar_geom_on, DatetimeUTC >= start_date & DatetimeUTC <= end_date)

#change to before interval
end_date <- start_date
start_date <- start_date - n_seconds

boar_geom_bon <- boar_data%>%
  st_as_sf(coords = c("E", "N"), crs = 2056)%>%
  filter(DatetimeUTC >= start_date & DatetimeUTC <= end_date)



#Calculating hex-grid binning for both time intervals
grid_boars_on <- hex_grids(boar_geom_on, boar_bbox, cell_size)
grid_boars_bon <- hex_grids(boar_geom_bon, boar_bbox, cell_size)%>%
  st_drop_geometry()

grid_boars_join <- left_join(grid_boars_on, grid_boars_bon, by = c("id_hex" = "id_hex"), suffix = c(".on",".bon"))%>%
  mutate(diff_on_bon = n.on - n.bon)%>%
  filter(n.bon != 0 | n.on != 0)%>%
  select(id_hex, n.bon, n.on, diff_on_bon, geometry)

  

#Setting up the visualisation
tmap_mode("view")
tmap_style("cobalt")

tm_shape(grid_boars_join)+
  tm_fill(col = "diff_on_bon",
              palette = brewer.pal(5, "RdYlGn"),
              style = "jenks",
              midpoint = 0,          
              alpha = 0.5,
              id = "diff_on_bon")+
tm_shape(wss_buffer1)+
  tm_borders(col = "grey",
          alpha = 0.5)+
  tm_text("rho_r1_on", ymod = 0.1, col = "white")+
tm_shape(donut_2)+
  tm_borders(col = "grey", 
          alpha = 0.5)+
  tm_text("rho_r2_on", ymod = 0.5, col = "white")+
tm_shape(donut_3)+
  tm_borders(col = "grey",
          alpha = 0.5)+
  tm_text("rho_r3_on", ymod = 1, col = "white")+
tm_shape(wss_geom)+
  tm_dots(col = "red")
  
  
