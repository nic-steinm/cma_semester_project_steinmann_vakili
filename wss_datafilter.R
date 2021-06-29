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
  filter(month(DatetimeUTC) >= 4 & month(DatetimeUTC) <= 10)#%>% #Changed thresholds so we get also data before and after the wildschweinschreck installation
#filter(TierName=="Ueli") #I would suggest using all individuals as this gives us more data to work with


write.csv(joined_tables_device_locs, file = "data/device_locations_filtered.csv")
write.csv(boar_locs_filtered, file = "data/boar_locations_filtered.csv")




# Speed
boar_locs_filtered <- group_by(boar_locs_filtered,TierID)
boar_locs_filtered$timelag  <- as.integer(difftime(lead(boar_locs_filtered$DatetimeUTC), boar_locs_filtered$DatetimeUTC, units = "sec"))
boar_locs_filtered$steplength <- as.numeric(sqrt((boar_locs_filtered$E - lead(boar_locs_filtered$E,1))^2 + (boar_locs_filtered$N - lead(boar_locs_filtered$N,1))^2))
boar_locs_filtered$speed <- as.numeric(boar_locs_filtered$steplength/boar_locs_filtered$timelag)
boar_locs_filtered <-  boar_locs_filtered %>%
  group_by(TierName) %>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))
boar_locs_filtered <- boar_locs_filtered %>% 
  group_by(TierName) %>%
  mutate(speed = steplength/timelag)
    

# Statistical Analysis
ggplot(boar_locs_filtered, aes(DatetimeUTC,TierID)) +
  geom_line()


ggplot(boar_locs_filtered, aes(timelag)) +
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
