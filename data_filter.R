library(devtools)
library(readr)
library(sf)
library(terra)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tmap)
library(sp)

# install.packages("devtools") # <- if you havent installed devtools already
devtools::install_github("ComputationalMovementAnalysis/ComputationalMovementAnalysisData")
library(ComputationalMovementAnalysisData)
head(wildschwein_BE)
head(wildschwein_metadata)
head(wildschwein_overlap_temp)
head(schreck_agenda)
head(schreck_locations)


#Used a little bit of a different approach for writing the LV95 coordinates, but this works too I guess :)
# Wildschweinschreck
wildschweinschreck_locations <- schreck_locations %>% #simplified passing on variables with piping so it's a bit more tidy looking 
  st_as_sf(coords = c("lon", "lat"), crs = CRS("+init=epsg:4326"), remove = FALSE)%>% 
  st_transform(crs = 2056)%>% 
  filter(lat < 47.2 & lon < 7.5) #Gibt es einen Grund für diesen Filter?

coord <- unlist(st_geometry(wildschweinschreck_locations))%>%
  matrix(ncol=2,byrow=TRUE) %>% as_tibble()%>%
  setNames(c("N","E"))

wildschweinschreck_locations$N <- coord$E
wildschweinschreck_locations$E <- coord$N
  
#join 
wildschweinschreck_locations <- wildschweinschreck_locations%>%
  left_join(schreck_agenda, by=c("id"="id"))

# Sample
head(wildschwein_BE)
head(wildschweinschreck_locations)

ueli <- wildschwein_BE%>% 
  filter(hour(DatetimeUTC) <= 6  |  hour(DatetimeUTC) >= 20)%>% #Corrected AND/OR statements: | = OR, & = AND, also used >= instead of > to include the month specified
  filter(month(DatetimeUTC) >= 4 & month(DatetimeUTC) <= 10)#%>% #Changed thresholds so we get also data before and after the wildschweinschreck installation
  #filter(TierName=="Ueli") #I would suggest using all individuals as this gives us more data to work with
