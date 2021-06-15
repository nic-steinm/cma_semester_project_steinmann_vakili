## writing the algorithm for range class density calculation
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
library(ComputationalMovementAnalysisData)


wss_distance <- function(x1, y1, x2, y2){
  sqrt((x1-x2)^2+(y1-y2)^2)
} 

r1 = 50
r2 = 100
r3 = 200
wss_data <- read_delim("data/device_locations_filtered.csv", delim = ",")

boar_data <- read_delim("data/boar_locations_filtered.csv", delim = ",")


#area sizes of range classes in ha
area_r1 <- round(((r1^2*pi) / 10000), 3)
area_r2 <- round((((r2^2*pi) - area_r1) / 10000), 3)
area_r3 <- round((((r3^2*pi) - (r2^2*pi)) / 10000), 3)
  
  #add columns for the results of the calculations
wss_data <- wss_data %>%
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
  

for (i in 1:(nrow(wss_data))){
    E_wss <- wss_data[i, 'E']
    N_wss <- wss_data[i, 'N']
    wss_start <- wss_data[i, 'datum_on']
    wss_end <- wss_data[i, 'datum_off']
    
    boar_data_filtered <- boar_data%>%
      filter(DatetimeUTC >= wss_start & DatetimeUTC <= wss_end)%>%
      mutate(dist = wss_distance(E_wss, N_wss, E, N))
    
    fixes_r1 <- boar_data_filtered%>%
      filter(dist <= r1)
    fixes_r2 <- boar_data_filtered%>%
      filter(dist > r1 & dist <= r2)
    fixes_r3 <- boar_data_filtered%>%
      filter(dist > r2 & dist <= r3)
    
    boar_data_filtered[i, 'n_r1'] <- nrow(fixes_r1)
    boar_data_filtered[i, 'n_r2'] <- nrow(fixes_r2)
    boar_data_filtered[i, 'n_r3'] <- nrow(fixes_r3)
    
    boar_data_filtered[i, 'rho_r1'] <- boar_data_filtered[i, 'n_r1']/area_r1
    boar_data_filtered[i, 'rho_r2'] <- boar_data_filtered[i, 'n_r2']/area_r2
    boar_data_filtered[i, 'rho_r3'] <- boar_data_filtered[i, 'n_r3']/area_r3
}
