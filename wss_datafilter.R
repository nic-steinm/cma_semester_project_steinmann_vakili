library(lubridate)
library(dplyr)
library(tibble)
library(sf)
library(ComputationalMovementAnalysisData)
library(readr)        

#Step 1: Reprojecting, Filtering and joining of deterrence device locations
head(schreck_agenda)
head(schreck_locations)

locations_sf <- st_as_sf(schreck_locations, coords = c('lon', 'lat'), crs = 4326)

lv95_coords <- locations_sf%>%
  st_transform(crs = 2056)%>%
  mutate(E = st_coordinates(.)[,1],
         N = st_coordinates(.)[,2])

#Joining agenda and locations and filtering by date
joined_tables_device_locs <- left_join(lv95_coords, schreck_agenda , by = c("id" = "id"))%>%
  mutate(datum_on = as_datetime(paste(as.character(datum_on), " 00:01:00")),
         datum_off = as_datetime(paste(as.character(datum_off), " 00:01:00")))%>%
  filter(as.integer(month(datum_on)) >= 5  &  as.integer(month(datum_on)) <= 9)

joined_tables_device_locs <- st_set_geometry(joined_tables_device_locs, NULL)

#Step 2: Filter the boar location data
head(wildschwein_BE)

boar_locs_filtered <- wildschwein_BE%>% 
  filter(hour(DatetimeUTC) <= 6  |  hour(DatetimeUTC) >= 20)%>% #Corrected AND/OR statements: | = OR, & = AND, also used >= instead of > to include the month specified
  filter(month(DatetimeUTC) >= 4 & month(DatetimeUTC) <= 10)%>%
  group_by(TierName)%>%
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")),
         steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2),
         speed = as.numeric(round(steplength/timelag, 2)))

write.csv(joined_tables_device_locs, file = "data/device_locations_filtered.csv")
write.csv(boar_locs_filtered, file = "data/boar_locations_filtered.csv")


  