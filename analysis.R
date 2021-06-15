library(devtools)
library(readr)
library(sf)
library(terra)
library(dplyr)
library(tibble)
library(lubridate)
library(ggplot2)
library(tmap)
library(sp)

#Step 1: Defining functions

#calculatin distance between device and boar fix
wss_distance <- function(x1, y1, x2, y2){
  sqrt((x1-x2)^2+(y1-y2)^2)
} 

#calculating stats for range classes
range_class <- function(wss_data, boar_data, r1, r2, r3){
  
  #area sizes of range classes in ha
  area_r1 <- (r1^2*pi) / 10000
  area_r2 <- ((r2^2*pi) - area_r1) / 10000
  area_r3 <- ((r3^2*pi) - (r2^2*pi)) / 10000
  
  #add columns for the results of the calculations
  wss_data %>%
    add_column(r1 = r1,
               r2 = r2,
               r3 = r3,
               a_r1 = area_r1,
               a_r2 = area_r2,
               a_r3 = area_r3,
               n_r1 = NA,
               n_r2 = NA,
               n_r3 = NA,
               rho_r1 = NA,
               rho_r2 = NA,
               rho_r3 = NA)
  
  
  for (row in 1:nrow(wss_data)){
    E_wss <- wss_data[row, 'E']
    N_wss <- wss_data[row, 'N']
    wss_start <- wss_data[row, 'datum_on']
    wss_end <- wss_data[row, 'datum_off']
    
    boar_data_filtered <- boar_data%>%
      filter(DatetimeUTC >= wss_start & DatetimeUTC <= wss_end)%>%
      mutate(dist = wss_distance(E_wss, N_wss, E, N))
    
    fixes_r1 <- boar_data_filtered%>%
      filter(dist <= r1)
    fixes_r2 <- boar_data_filtered%>%
      filter(dist > r1 & dist <= r2)
    fixes_r3 <- boar_data_filtered%>%
      filter(dist > r2 & dist <= r3)
    
    boar_data_filtered[row, 'n_r1'] <- nrow(fixes_r1)
    boar_data_filtered[row, 'n_r2'] <- nrow(fixes_r2)
    boar_data_filtered[row, 'n_r3'] <- nrow(fixes_r3)
    
    boar_data_filtered[row, 'rho_r1'] <- boar_data_filtered[row, 'n_r1']/area_r1
    boar_data_filtered[row, 'rho_r2'] <- boar_data_filtered[row, 'n_r2']/area_r2
    boar_data_filtered[row, 'rho_r3'] <- boar_data_filtered[row, 'n_r3']/area_r3
  }
}

#Step 2: Loading in data sources, defining parameters
wss_locs <- read_delim("data/device_locations_filtered.csv", delim = ",")%>%
  select(-geometry) #dropping the geometry column once and for all

boar_locs <- read_delim("data/boar_locations_filtered.csv", delim = ",")

#defining range class radii in meters moving from close to far range
rad_1 <- 20
rad_2 <- 50
rad_3 <- 100


#Distance algorithm running
test_output <- range_class(wss_locs, boar_locs, rad_1, rad_2, rad_3)


#joining data to table