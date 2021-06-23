library(lubridate)
library(dplyr)
library(tibble)
library(SpatialKDE)
library(sp)
library(sf)
library(readr)
library(tmap)
library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "rad1",
              label = "Choose radius 1",
              value = 50, min = 1, max = 1000),
  
  sliderInput(inputId = "rad2",
              label = "Choose radius 2",
              value = 150, min = 1, max = 1000),
  
  sliderInput(inputId = "rad3",
              label = "Choose radius 3",
              value = 300, min = 1, max = 1000),
  
  tmapOutput("my_tmap")
)


server <- function(input, output){

  wss_data <- read_delim("data/range_class_data.csv", delim = ",")
  boar_data <- read_delim("data/boar_locations_filtered.csv", delim = ",")
  
  wss_geom <- wss_data%>%
    st_as_sf(coords = c("E", "N"), crs = 2056)
  
  boar_geom <- boar_data%>%
    st_as_sf(coords = c("E", "N"), crs = 2056)
  
  #calculating buffers
  observe({
    wss_buffer1 <- st_buffer(wss_geom, dist = 50)
    wss_buffer2 <- st_buffer(wss_geom, dist = 150)
    wss_buffer3 <- st_buffer(wss_geom, dist = 300)
  })
  
  #KDE calculation
  cell_size <- 200
  band_width <- 200
  
  grid_boars <- boar_geom%>%
    create_grid_hexagonal(cell_size = cell_size, side_offset = band_width)
  
  kde_boars <- boar_geom%>%
    kde(band_width = band_width, kernel = "quartic", grid = grid_boars)%>%
    filter(kde_value != 0)

  tmap_mode("view")
  
 output$my_tmap = renderTmap({ tm_shape(kde_boars)+
    tm_polygons(col = "kde_value", palette = "viridis", alpha = 0.3, style = "jenks")+
    tm_shape(wss_buffer3)+
    tm_polygons(col = "black", alpha = 0.1) + 
    tm_shape(wss_buffer2)+
    tm_polygons(col = "black", alpha = 0.1)+
    tm_shape(wss_buffer1)+
    tm_polygons(col = "black", alpha = 0.1)
 })
}

shinyApp(ui = ui, server = server)