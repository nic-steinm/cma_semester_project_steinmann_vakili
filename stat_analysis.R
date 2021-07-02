library(readr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tibble)

wss_data <- read_delim("data/range_class_data.csv", delim = ",")
            
  
  
#Changing data table to long format
wss_data_long <- wss_data%>%  
            pivot_longer(c('meansl_r1_on',
                           'meansl_r2_on',
                           'meansl_r3_on',
                           'meansl_r1_bon',
                           'meansl_r2_bon',
                           'meansl_r3_bon',
                           'rho_r1_on',
                           'rho_r2_on',
                           'rho_r3_on',
                           'rho_r1_bon',
                           'rho_r2_bon',
                           'rho_r3_bon'), 
                          names_to = "name", values_to = "value")




#T-tests mean steplength [m]

#range class one
steplength_r1_on <- wss_data$meansl_r1_on
steplength_r1_bon <- wss_data$meansl_r1_bon
t.test(x = steplength_r1_on, y = steplength_r1_bon, paired = TRUE, conf.level = 0.95)

bp <- filter(wss_data_long, name == "meansl_r1_on" | name == "meansl_r1_bon")
ggplot(bp)+
  geom_boxplot(aes(name, value, color = name), na.rm = TRUE)+
  labs(title = "Steplength comparison between range class 1 under different WSS status")+
  ylab("Steplength [m]")+
  xlab("Activation status [on/before-on]")


#range class two
steplength_r2_on <- wss_data$meansl_r2_on
steplength_r2_bon <- wss_data$meansl_r2_bon
t.test(x = steplength_r2_on, y = steplength_r2_bon, paired = TRUE, conf.level = 0.95)

bp <- filter(wss_data_long, name == "meansl_r2_on" | name == "meansl_r2_bon")
ggplot(bp)+
  geom_boxplot(aes(name, value, color = name), na.rm = TRUE)+
  labs(title = "Steplength comparison between range class 2 under different WSS status")+
  ylab("Steplength [m]")+
  xlab("Activation status [on/before-on]")
  
#range class three
steplength_r3_on <- wss_data$meansl_r3_on
steplength_r3_bon <- wss_data$meansl_r3_bon
t.test(x = steplength_r3_on, y = steplength_r3_bon, paired = TRUE, conf.level = 0.95)

bp <- filter(wss_data_long, name == "meansl_r3_on" | name == "meansl_r3_bon")
ggplot(bp)+
  geom_boxplot(aes(name, value, color = name), na.rm = TRUE)+
  labs(title = "Steplength comparison between range class 3 under different WSS status")+
  ylab("Steplength [m]")+
  xlab("Activation status [on/before-on]")






#T-tests rho [number of fixes per ha and day]

#range class one
rho_r1_on <- wss_data$rho_r1_on
rho_r1_bon <- wss_data$rho_r1_bon
t.test(x = rho_r1_on, y = rho_r1_bon, paired = TRUE, conf.level = 0.95)

bp <- filter(wss_data_long, name == "rho_r1_on" | name == "rho_r1_bon")
ggplot(bp)+
  geom_boxplot(aes(name, value, color = name), na.rm = TRUE)+
  labs(title = "Comparison of fix density between range class 1 under different WSS status")+
  ylab("Mean density [n_fixes/ha/day]")+
  xlab("Activation status [on/before-on]")

#range class two
rho_r2_on <- wss_data$rho_r2_on
rho_r2_bon <- wss_data$rho_r2_bon

t.test(x = rho_r2_on, y = rho_r2_bon, paired = TRUE, conf.level = 0.95)


bp <- filter(wss_data_long, name == "rho_r2_on" | name == "rho_r1_bon")
ggplot(bp)+
  geom_boxplot(aes(name, value, color = name), na.rm = TRUE)+
  labs(title = "Comparison of fix density between range class 2 under different WSS status")+
  ylab("Mean density [n_fixes/ha/day]")+
  xlab("Activation status [on/before-on]")

#range class three
rho_r3_on <- wss_data$rho_r3_on
rho_r3_bon <- wss_data$rho_r3_bon

t.test(x = rho_r3_on, y = rho_r3_bon, paired = TRUE, conf.level = 0.95)

bp <- filter(wss_data_long, name == "rho_r3_on" | name == "rho_r3_bon")
ggplot(bp)+
  geom_boxplot(aes(name, value, color = name), na.rm = TRUE)+
  labs(title = "Comparison of fix density between range class 3 under different WSS status")+
  ylab("Mean density [n_fixes/ha/day]")+
  xlab("Activation status [on/before-on]")

