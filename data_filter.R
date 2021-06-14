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


# Wildschweinschreck
wildschweinschreck_locations <- schreck_locations %>% st_as_sf(coords = c("lon", "lat"), crs = CRS("+init=epsg:4326"), remove = FALSE)  #%>% st_transform(crs = 2056)
wildschweinschreck_locations <- wildschweinschreck_locations %>% st_transform(crs = 2056)
wildschweinschreck_locations <- wildschweinschreck_locations %>% filter(lat < 47.2 & lon < 7.5)
coord <- unlist(st_geometry(wildschweinschreck_locations)) %>% matrix(ncol=2,byrow=TRUE) %>% as_tibble() %>% setNames(c("N","E"))
wildschweinschreck_locations$N <- coord$E
wildschweinschreck_locations$E <- coord$N
#join 
wildschweinschreck_locations <- wildschweinschreck_locations %>% left_join(schreck_agenda, by=c("id"="id"))

# Sample
head(wildschwein_BE)
head(wildschweinschreck_locations)
ueli <- wildschwein_BE %>% filter(TierName=="Ueli")
ueli <- ueli %>% filter(hour(DatetimeUTC)< 6 | hour(DatetimeUTC) > 20)
ueli <- ueli %>% filter(month(DatetimeUTC)< 5 | month(DatetimeUTC) > 9)

