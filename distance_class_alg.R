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

  
#Defining distance parameters in metres, loading data
r1 = 50
r2 = 100
r3 = 500

wss_data <- read_delim("data/device_locations_filtered.csv", delim = ",")

boar_data <- read_delim("data/boar_locations_filtered.csv", delim = ",")

#Running the algorithm for switched on wildschweinschreck

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
             n_r1_on = 0,
             n_r2_on = 0,
             n_r3_on = 0,
             rho_r1_on = 0,
             rho_r2_on = 0,
             rho_r3_on = 0)


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
    wss_data$n_r1_on[i] <- nrow(fixes_r1)
    wss_data$n_r2_on[i] <- nrow(fixes_r2)
    wss_data$n_r3_on[i] <- nrow(fixes_r3)
  
    #boar density per hectare and day, to make relative numbers comparable
    wss_data$rho_r1_on[i] <- round((wss_data$n_r1_on[i]/area_r1)/n_days, 2)
    wss_data$rho_r2_on[i] <- round((wss_data$n_r2_on[i]/area_r2)/n_days, 2)
    wss_data$rho_r3_on[i] <- round((wss_data$n_r3_on[i]/area_r3)/n_days, 2)
    }

#filtering data entries with no boar locations in r1
wss_data <- wss_data%>%
  filter(n_r1_on != 0)

#Running the algorithm for time before switched on

wss_data <- wss_data %>%
  add_column(n_r1_bon = 0, # -bon suffix stands for "before on"
             n_r2_bon = 0,
             n_r3_bon = 0,
             rho_r1_bon = 0,
             rho_r2_bon = 0,
             rho_r3_bon = 0)


for (i in 1:nrow(wss_data)){
  E_wss <- wss_data$E[i]
  N_wss <- wss_data$N[i]
  wss_start <- wss_data$datum_on[i] - as.numeric(wss_data$datum_off[i] - wss_data$datum_on[i])*86400 #using the same time interval for before as during device activation
  wss_end <- wss_data$datum_on[i]
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
  wss_data$n_r1_bon[i] <- nrow(fixes_r1)
  wss_data$n_r2_bon[i] <- nrow(fixes_r2)
  wss_data$n_r3_bon[i] <- nrow(fixes_r3)
  
  #boar density per hectare and day
  wss_data$rho_r1_bon[i] <- round((wss_data$n_r1_bon[i]/area_r1)/n_days, 2)
  wss_data$rho_r2_bon[i] <- round((wss_data$n_r2_bon[i]/area_r2)/n_days, 2)
  wss_data$rho_r3_bon[i] <- round((wss_data$n_r3_bon[i]/area_r3)/n_days, 2)
}

write.csv(wss_data, file = "data/range_class_data.csv")
