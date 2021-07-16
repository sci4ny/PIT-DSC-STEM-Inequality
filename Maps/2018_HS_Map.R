library(leaflet)
library(sp)
library(tidyverse)
library(dplyr)
library(MASS)

#load in dataset
data <- read.csv("~/PIT-DSC-STEM-Inequality/data/2018_HS_Data.csv")

#turn %poverty column into character for better map display
data[,47] <- data[,47] * 100
data[,47] <- paste(data[,47], "%")

#turn decimal number in the ratio column into fractions, then turn fractions into ratios
data[,31] <- paste(fractions(data[,31]), " ")
data[,31] <- str_replace(data[,31],"/", ":")

#want first number to be latitude, second numer to be longitude
data.SP <- SpatialPointsDataFrame(data[,c(5,6)], data[,-c(5,6)])


#leaflet object with map tile
hs_map <- leaflet() %>%
     addTiles() %>%
     addCircleMarkers(data = data, lng = ~Longitude, lat = ~Latitude, color = "#2a52be",
                      popup = ~paste("<h3 style = 'color: #2a52be'>School Information</h3>",
                                     "<b>School Name: </b>",
                                     school_name,
                                     "<br>",
                                     "<b>% of Students in Poverty: </b>",
                                     Percent.Poverty,
                                     "<br>",
                                     "<b> Ratio of STEM teachers to Students: </b>",
                                     Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students,
                                     "<br>",
                                     "<b> STEM AP Courses Offered: </b>",
                                     STEM_AP_Courses_Offered,
                                     sep = " "))

#showcase map
hs_map
