#This is for our global variables/data
library(dplyr)
library(MASS)
library(stringr)

#load hs data
hs_data <- read.csv("~/PIT-DSC-STEM-Inequality/data/2018_HS_Data.csv")

#turn decimal number in the ratio column into fractions, then turn fractions into ratios
hs_data[,31] <- paste(fractions(hs_data[,31], cycles = 3), " ")
hs_data[,31] <- str_replace(hs_data[,31],"/", ":")

#remove whitespace from end of string and front of string
hs_data[,"Borough"] <- hs_data[,"Borough"] %>%  str_trim(side = "both")

#adds salary per classroom column
hs_data <- hs_data %>% mutate('Salary Per Classroom Teacher' = Classroom.Teachers_2017.18
                    / (Classroom.Teachers.w..0.3.Years.Experience_2017.18 + Classroom.Teachers.w..More.than.3.Years.Experience_2017.18)
)

#adds commas to better display larger numbers (1000 turns to 1,000)
hs_data[,45] <- format(hs_data[,45], big.mark = ",", scientific = FALSE)
hs_data[,50] <- format(hs_data[,50], big.mark = ",", scientific = FALSE)

#changes poverty percent to non-decimals (.91 turn to 91)
hs_data[,47] <- hs_data[,47] * 100
