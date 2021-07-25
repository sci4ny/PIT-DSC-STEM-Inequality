#This is for our global variables/data
library(dplyr)
library(MASS)
library(stringr)
library(readxl)

#load our school datasets
hs_data <- read.csv("~/PIT-DSC-STEM-Inequality/data/2018_HS_Data.csv")
ms_df <- read_excel('~/PIT-DSC-STEM-Inequality/data/ratiochange.xlsx')
elem_data <- read_excel("~/PIT-DSC-STEM-Inequality/data/elementarymapdata.xlsx")

#turn decimal number in the ratio column into fractions, then turn fractions into ratios
hs_data[,31] <- paste(fractions(hs_data[,31], cycles = 3), " ")
hs_data[,31] <- str_replace(hs_data[,31],"/", ":")

#for elementary dataset, first have to unlist
elem_data$RTS <- unlist(elem_data$RTS)
elem_data$RTS <- paste(fractions(elem_data$RTS, cycles = 3), " ")
elem_data$RTS <- str_replace(elem_data$RTS, "/", ":")

ms_df <- subset(ms_df, select = -c(9) )

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
elem_data[,47] <- elem_data[,47] * 100

#we want to add an elementary column to the elem_data
elem_data$Borough <- NA

#want to go through every row and based on dbn determine the which borough the
#school resides in
for (i in 1:nrow(elem_data)) {

     #depending on the letter of the DBN, we assign it the appropriate borough

     if (substr(elem_data$DBN[i],3,3) == "X") {elem_data$Borough[i] <- "BRONX"}
     else if (substr(elem_data$DBN[i],3,3) == "M") {elem_data$Borough[i] <- "MANHATTAN"}
     else if (substr(elem_data$DBN[i],3,3) == "K") {elem_data$Borough[i] <- "BROOKLYN"}
     else if (substr(elem_data$DBN[i],3,3) == "Q") {elem_data$Borough[i] <- "QUEENS"}
     else if (substr(elem_data$DBN[i],3,3) == "R") {elem_data$Borough[i] <- "STATEN IS"}


          }

