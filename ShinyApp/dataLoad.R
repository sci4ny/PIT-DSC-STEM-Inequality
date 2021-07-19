#This is for our global variables/data
library(dplyr)

#load hs data
hs_data <- read.csv("~/PIT-DSC-STEM-Inequality/data/2018_HS_Data.csv")

#turn decimal number in the ratio column into fractions, then turn fractions into ratios
hs_data[,31] <- paste(fractions(hs_data[,31], cycles = 3), " ")
hs_data[,31] <- str_replace(hs_data[,31],"/", ":")


