## writing the algorithm for range class density calculation
library(dplyr)
library(lubridate)

wss_distance <- function(x1, y1, x2, y2){
  sqrt((x1-x2)^2+(y1-y2)^2)
} 
  

range_class <- function(wss_data, boar_data, r1, r2, r3){
  
  #area sizes of range classes in ha
  area_r1 <- (r1^2*pi) / 10000
  area_r2 <- ((r2^2*pi) - area_r1) / 10000
  area_r3 <- ((r3^2*pi) - (r2^2*pi)) / 10000
  
  #data frame for output
  output_frame <- data.frame(wss_id = character(),
                             r1 = integer(),
                             r2 = integer(),
                             r3 = integer(),
                             nfix_r1 = double(),
                             nfix_r2 = double(),
                             nfix_r3 = double(),
                             dens_r1 = double(),
                             dens_r2 = double(),
                             dens_r3 = double()
                             )
  
  for (row in 1:nrow(wss_data)){
    E_wss <- wss_data[row, 'E']
    N_wss <- wss_data[row, 'N']
    id <- wss_data[row, 'id']
    wss_start <- wss_data[row, 'datum_on']
    wss_end <- wss_data[row, 'datum_off']
    
    boar_data_filtered <- boar_data%>%
      filter(as_date(DateTimeUTC) >= as_date(wss_start),
              as_date(DateTimeUTC) <= as_date(wss_end))%>%
      mutate(dist = wss_distance(E_wss, N_wss, E, N))
      
    fixes_r1 <- boar_data_filtered%>%
      filter(dist <= r1,)
    fixes_r2 <- boar_data_filtered%>%
      filter(dist > r1, dist <= r2)
    fixes_r3 <- boar_data_filtered%>%
      filter(dist > r2, dist <= r3)
    
    n_r1 <- length(fixes_r1)
    n_r2 <- length(fixes_r2)
    n_r3 <- length(fixes_r3)
    
    rho_r1 <- n_r1/area_r1
    rho_r2 <- n_r2/area_r2
    rho_r3 <- n_r3/area_r3
    
    add_row(output_frame,
            wss_id = id,
            r1 = r1,
            r2 = r2,
            r3 = r3,
            nfix_r1 = n_r1, 
            nfix_r2 = n_r2,
            nfix_r3 = n_r3,
            dens_r1 = rho_r1,
            dens_r2 = rho_r2,
            dens_r3 = rho_r3)
  }
  
  return(output_frame)
}


