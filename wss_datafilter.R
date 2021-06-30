library(lubridate)
library(dplyr)
library(tibble)
library(sf)
library(ComputationalMovementAnalysisData)
library(readr)        
library(ggplot2)      

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



# Statistical Analysis
ggplot(boar_locs_filtered, aes(DatetimeUTC,TierName), color = ) +
  geom_line()


ggplot(boar_locs_filtered, aes(timelag)) + #I don't know what that is supposed to tell
  geom_histogram(binwidth = 50) +
  lims(x = c(0,15000)) +
  scale_y_log10()


boar_locs_filtered %>%
  filter(year(DatetimeUTC)  == 2014) %>%
  ggplot(aes(DatetimeUTC,timelag, colour = TierID)) +
  geom_line() +
  geom_point()

boar_locs_filtered %>%
  filter(year(DatetimeUTC)  == 2015) %>%
  ggplot(aes(DatetimeUTC,timelag, colour = TierID)) +
  geom_line() +
  geom_point()

boar_locs_filtered %>%
  filter(year(DatetimeUTC)  == 2016) %>%
  ggplot(aes(DatetimeUTC,timelag, colour = TierID)) +
  geom_line() +
  geom_point()

