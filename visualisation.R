library(lubridate)
library(dplyr)
library(tibble)
library(SpatialKDE)
library(sp)
library(sf)
library(readr)
library(tmap)
library(shiny)

wss_data <- read_delim("data/range_class_data.csv", delim = ",")
boar_data <- read_delim("data/boar_locations_filtered.csv", delim = ",")



ui <- fluidPage(
  
  sliderInput("cellsize", "Choose cell size [m]", min = 100, max = 750, value = 200),
  selectInput("device", "Choose deterrence device:", c(1:nrow(wss_data))),
  
  tmapOutput("boarmap")
  )


server <- function(input, output, session){
  
  #functions
  hex_grids <- function(geom, cell_size){
  
  grid <- geom%>%
    create_grid_hexagonal(cell_size = cell_size)%>%
    st_sf()%>%
    mutate(id_hex = 1:n())%>%
    dplyr::select(id_hex, geometry)
  
  grid_geom_joined <- st_join(geom, grid)
  grid_count <- grid_geom_joined%>%
    st_drop_geometry()%>%
    count(id_hex)
  
  hex_grid_geom <- grid%>%
    left_join(grid_count)%>%
    filter(!is.na(n))
  
  return(hex_grid_geom)
}

  #converting to geometry  
  wss_geom <- wss_data%>%
    st_as_sf(coords = c("E", "N"), crs = 2056)
  
  boar_geom <- boar_data%>%
    st_as_sf(coords = c("E", "N"), crs = 2056)
  
  #calculating ranges
  wss_buffer1 <- st_buffer(wss_geom, dist = 50)
  wss_buffer2 <- st_buffer(wss_geom, dist = 150)
  wss_buffer3 <- st_buffer(wss_geom, dist = 300)
  
 
  #honeycomb grid binning
  cell_size <- 200

  grid_boars <- hex_grids(boar_geom, cell_size)

  
  
  tmap_mode("view")
  
  #tmap-mapping
  output$boarmap <- renderTmap({
    tmap_style("cobalt")+
    tm_shape(grid_boars)+
      tm_fill(col = "n",
            alpha = 0.5,
            palette = "viridis",
            style = "jenks",
            id = "n",
            legend.hist = TRUE,
            zindex = 1)+
    tm_shape(wss_geom)+
      tm_dots(col= "green")
    })
  
    observe({
      
      wss_geom <- wss_data%>%
        st_as_sf(coords = c("E", "N"), crs = 2056)
      
      boar_geom <- boar_data%>%
        st_as_sf(coords = c("E", "N"), crs = 2056)
     
      #honeycomb grid binning
      cell_size <- input$cellsize
      
      grid_boars <- hex_grids(boar_geom, cell_size)
        
      #tmap_proxy
      tmapProxy("boarmap", session,{
        tm_shape(grid_boars)+
          tm_remove_layer(1)+
          tm_fill(col = "n",
                  alpha = 0.5,
                  palette = "viridis",
                  style = "jenks",
                  id = "n",
                  legend.hist = TRUE,
                  zindex = 1)
      })
      
    })
}

app <- shinyApp(ui, server)