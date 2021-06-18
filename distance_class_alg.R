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

#Defining functions
wss_distance <- function(x1, y1, x2, y2){
  sqrt((x1-x2)^2+(y1-y2)^2)
} 

distance_class <- function(wss_data, boar_data, r1, r2, r3){
  
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
               n_r1 = 0,
               n_r2 = 0,
               n_r3 = 0,
               rho_r1 = 0,
               rho_r2 = 0,
               rho_r3 = 0)
  
  
  for (i in 1:nrow(wss_data)){
    E_wss <- wss_data$E[i]
    N_wss <- wss_data$N[i]
    wss_start <- wss_data$datum_on[i]
    wss_end <- wss_data$datum_off[i]
    n_days <- as.numeric(difftime(wss_end, wss_start, units = "days"))
    
    boar_data_filtered <- boar_data%>%
      filter(DatetimeUTC >= wss_start & DatetimeUTC <= wss_end)%>%
      mutate(dist = wss_distance(E_wss, N_wss, E, N))
    
    fixes_r1 <- boar_data_filtered%>%
      filter(dist <= r1)
    fixes_r2 <- boar_data_filtered%>%
      filter(dist > r1 & dist <= r2)
    fixes_r3 <- boar_data_filtered%>%
      filter(dist > r2 & dist <= r3)
    
    #number of fixes in each range class and specified time interval
    wss_data$n_r1[i] <- nrow(fixes_r1)
    wss_data$n_r2[i] <- nrow(fixes_r2)
    wss_data$n_r3[i] <- nrow(fixes_r3)
    
    #boar density per hectare and day
    wss_data$rho_r1[i] <- round((wss_data$n_r1[i]/area_r1)/n_days, 2)
    wss_data$rho_r2[i] <- round((wss_data$n_r2[i]/area_r2)/n_days, 2)
    wss_data$rho_r3[i] <- round((wss_data$n_r3[i]/area_r3)/n_days, 2)
  }
}

#Defining distance parameters, loading data
rad_1 = 100
rad_2 = 250
rad_3 = 2500

wss_locs <- read_delim("data/device_locations_filtered.csv", delim = ",")

boar_locs <- read_delim("data/boar_locations_filtered.csv", delim = ",")

#Running the algorithm for switched on wildschweinschreck
test <- distance_class(wss_locs, boar_locs, rad_1, rad_2, rad_3)

#Running the algorithm for time before switched on