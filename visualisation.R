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
  mutate(id_name = paste(as.character(X1), ":", " ", flurname, sep = ""))

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



#Choose your values
wss_data

#Select the device by number
device_number <- 7

#Select cell size in metres (be advised: cell size < 100 takes some time to calculate, < 50 not recommended)
cell_size <- 100



#converting to geometry
wss_geom <- wss_data%>%
  st_as_sf(coords = c("E", "N"), crs = 2056)%>%
  filter(X1 == device_number)

rad1 = wss_geom$r1
rad2 = wss_geom$r2
rad3 = wss_geom$r3

#Setting Buffers
rad1 = wss_geom$r1
rad2 = wss_geom$r2
rad3 = wss_geom$r3

wss_buffer1 <- st_buffer(wss_geom, dist = rad1)
wss_buffer2 <- st_buffer(wss_geom, dist = rad2)
wss_buffer3 <- st_buffer(wss_geom, dist = rad3)

#calculating difftime
start_date <- wss_geom$datum_on
end_date <- wss_geom$datum_off
n_seconds <- as.numeric(difftime(end_date, start_date, units = "secs"))

 
#honeycomb grid binning on status
boar_geom_on <- st_as_sf(boar_data, coords = c("E", "N"), crs = 2056)
boar_bbox <- st_as_sf(st_make_grid(boar_geom_on, n = 1))
boar_geom_on <- filter(boar_geom_on, DatetimeUTC >= start_date & DatetimeUTC <= end_date)

#change to before interval
end_date <- start_date
start_date <- start_date - n_seconds

#honeycomb grid binning before on status
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
              palette = brewer.pal(4, "RdBu"),
              style = "cont",
              midpoint = 0,          
              alpha = 0.7,
              id = "diff_on_bon"
              )+
tm_shape(wss_buffer1)+
  tm_borders(col = "grey",
          alpha = 0.7)+
tm_shape(wss_buffer2)+
  tm_borders(col = "grey", 
          alpha = 0.7)+
tm_shape(wss_buffer3)+
  tm_borders(col = "grey",
          alpha = 0.7)+
tm_shape(wss_geom)+
  tm_dots(col = "red",
          id = "id",
          popup.vars = c(
            "Region:" = "region",
            "Crop:" = "kultur",
            "Activation:" = "datum_on",
            "Deactivation:" = "datum_off",
            "Radius r1:" = "r1",
            "Radius r2:" = "r2",
            "Radius r3:" = "r3",
            "Density r1 before active:" = "rho_r1_bon",
            "Density r1 while active:" = "rho_r1_on",
            "Density r2 before active:" = "rho_r2_bon",
            "Density r2 while active:" = "rho_r2_on",
            "Density r3 before active:" = "rho_r3_bon",
            "Density r3 while active:" = "rho_r3_on",
            "Mean step length r1 before active:" = "meansl_r1_bon",
            "Mean step length r1 while active:" = "meansl_r1_on",
            "Mean step length r2 before active:" = "meansl_r2_bon",
            "Mean step length r2 while active:" = "meansl_r2_on",
            "Mean step length r3 before active:" = "meansl_r3_bon",
            "Mean step length r3 while active:" = "meansl_r3_on"
          ))
  


# Visualisation #2 for general overview


#loading function
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

wss_geom <- wss_data%>%
  st_as_sf(coords = c("E", "N"), crs = 2056)%>%
  distinct(id, .keep_all= TRUE)


boar_geom <- st_as_sf(boar_data, coords = c("E", "N"), crs = 2056)
boar_bbox <- st_as_sf(st_make_grid(boar_geom, n = 1))

grid_boars <- hex_grids(boar_geom, boar_bbox, 200)%>%
  filter(n != 0)

tmap_mode("view")
tmap_style("cobalt")

tm_shape(grid_boars)+
  tm_fill(col = "n",
          palette = "viridis",
          style = "kmeans",
          alpha = 0.3,
          id = "n"
          )+
tm_shape(wss_geom)+
  tm_dots(col = "red",
          id = "id",
          popup.vars = c(
            "Region:" = "region",
            "Place Name:" = "flurname",
            "Crop:" = "kultur")
          )
          
