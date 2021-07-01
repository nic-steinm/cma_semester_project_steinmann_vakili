library(readr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tibble)

wss_data <- read_delim("data/range_class_data.csv", delim = ",")
            
  
  
#Changing data table to long format
steplength <- wss_data%>%  
            pivot_longer(c('meansl_r1_on',
                           'meansl_r2_on',
                           'meansl_r3_on',
                           'meansl_r1_bon',
                           'meansl_r2_bon',
                           'meansl_r3_bon'), 
                          names_to = "name", values_to = "value")

rho <- wss_data%>%
            pivot_longer(c('rho_r1_on',
                          'rho_r2_on',
                          'rho_r3_on',
                          'rho_r1_bon',
                          'rho_r2_bon',
                          'rho_r3_bon'),
                          names_to = "name", values_to = "value")


#Plotting boxplot
ggplot(rho)+
  geom_boxplot(aes(name, value), na.rm = TRUE)

ggplot(steplength)+
  geom_boxplot(aes(name, value), na.rm = TRUE)

#T-tests
t_test_data <- wss_data

x_test <- t_test_data$rho_r3_on
y_test <- t_test_data$rho_r3_bon

t.test(x = x_test, y = y_test, alt = "less", conf.level = 0.95)
