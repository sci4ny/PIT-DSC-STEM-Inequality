library(tidyverse)
library(dplyr)

#load in dataset
data_2018 <- read.csv("~/DataClean/mapping/2018_DOE_High_School_Directory.csv")


#take only AP courses
ap_courses <- data_2018 %>% select("advancedplacement_courses")

#we get all the unique values in our column and combine them together as a long
#string
ap_courses_string <-  toString(unique(ap_courses$advancedplacement_courses))

#split up the string into a character vector so we can apply unique function
ap_courses_string <- strsplit(ap_courses_string, ", ")

#makes a vector of all the possible AP classes that is in our dataset
unique_ap <- unique(unlist(ap_courses_string))

#remove the empty element
unique_ap <- unique_ap[unique_ap != ""]

#create data we want to add our new AP columns in
ap_data <- data_2018 %>% select(1:3,"advancedplacement_courses")

#add new columns with initially NA values
ap_data[unique_ap] <- NA

#we want to loop through each row, hence 1:nrow(ap_data)
for (p in 1:nrow(ap_data)){
     #we wanted to check if the row contains our AP course in its string
     for (j in unique_ap){
          #we want to populate the pth row, in column j with either TRUE (1) or
          #FALSE (0), so this checks if the AP course is apart of the string
          ap_data[p,j] <- as.numeric(grepl(j, ap_data$advancedplacement_courses[p], fixed = TRUE) == TRUE)
     }
}

#remove non-stem AP courses!
ap_data <- ap_data[c(1:3,7:10,9,12,14,16,19,25)]

#create column with total stem courses offered
ap_data <- ap_data %>% mutate(STEM_AP_Courses_Offered = `AP Chemistry` + `AP Environmental Science` +
                                   `AP Biology` +  `AP Statistics` + `AP Computer Science` +
                                   `AP Calculus AB` + `AP Calculus BC` + `AP Physics`)


#we want to only take dbn column and stem courses offered column so that we can join it with our dataset
ap_data <- ap_data %>% select(c(1,14))

#save dataset as csv file
#write.csv(ap_data, "AP2018_HS.csv")
