library(lubridate)
library(dplyr)
library(sf)


#Step 1: Reprojecting, Filtering and joining of deterrence device locations
agenda <- ComputationalMovementAnalysisData::schreck_agenda
locations <- ComputationalMovementAnalysisData::schreck_locations

locations_sf <- st_as_sf(locations, coords = c('lon', 'lat'), crs = 4326)%>%
  
lv95_coords <- locations_sf%>%
  st_transform(crs = 2056)%>%
  mutate(E = st_coordinates(.)[,1],
         N = st_coordinates(.)[,2])

#Filtering by
joined_tables_device_locs <- left_join(lv95_coords, agenda, by = c("id" = "id"))%>%
  filter(as.integer(month(as_date(datum_on))) >= 5  &  as.integer(month(as_date(datum_on))) <= 9)

#

write.csv(joined_tables_device_locs, file = "data/device_locations_filtered.csv")
write.csv(boar_locs_filtered, file = "data/boar_locations_filtered.csv")