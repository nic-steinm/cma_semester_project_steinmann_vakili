library(lubridate)
library(dplyr)
library(sf)


agenda <- ComputationalMovementAnalysisData::schreck_agenda
locations <- ComputationalMovementAnalysisData::schreck_locations


locations_sf <- st_as_sf(locations, coords = c('lon', 'lat'), crs = 4326)%>%
  
lv95_coords <- locations_sf%>%
  st_transform(crs = 2056)%>%
  mutate(E = st_coordinates(.)[,1],
         N = st_coordinates(.)[,2])


joined_tables <- left_join(lv95_coords, agenda, by = c("id" = "id"))%>%
  filter(as.integer(month(as_date(datum_on))) >= 5  &  as.integer(month(as_date(datum_on))) <= 9)


write.csv(joined_tables, file = "data/wss_locations.csv")