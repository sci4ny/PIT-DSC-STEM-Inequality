#This is for our global variables/data
library(dplyr)
library(MASS)
library(stringr)
library(readxl)

#load our school datasets
hs_data <- read.csv("~/PIT-DSC-STEM-Inequality/data/2018_HS_Data.csv")
ms_df <- read_excel('~/PIT-DSC-STEM-Inequality/data/ratiochange.xlsx')
elem_data <- read_excel("~/PIT-DSC-STEM-Inequality/data/elementarymapdata.xlsx")
master_data <- read.csv("~/PIT-DSC-STEM-Inequality/data/2018_Master_Data.csv")

#turn decimal number in the ratio column into fractions, then turn fractions into ratios
hs_data[,31] <- paste(fractions(hs_data[,31], cycles = 3), " ")
hs_data[,31] <- str_replace(hs_data[,31],"/", ":")

#do same for master_data
master_data$Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students <- paste(fractions(master_data[,18], cycles = 3), " ")
master_data$Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students <- str_replace(master_data[,18],"/",":")

#for elementary dataset, first have to unlist
elem_data$RTS <- unlist(elem_data$RTS)
elem_data$RTS <- paste(fractions(elem_data$RTS, cycles = 3), " ")
elem_data$RTS <- str_replace(elem_data$RTS, "/", ":")

#

ms_df <- subset(ms_df, select = -c(9) )

#remove whitespace from end of string and front of string
hs_data[,"Borough"] <- hs_data[,"Borough"] %>%  str_trim(side = "both")
master_data[,"Borough"] <- master_data[,"Borough"] %>% str_trim(side = "both")

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


#master data gradespan column cleaning
#want to clean the row values with (School will serve...) [row 62,156,191,762,861,1080]


master_data[c(62,156,191,762,861,1080),"gradespan"] <- master_data[c(62,156,191,762,861,1080),"gradespan"] %>%
        #we get ride of "School will serve" by taking the substring of these rows, starting with the first
        #value of the string through until we hit the pattern "School", and then substracting by two so that we
        #don't include the parenthesis "(school Will serve...)"
        substr(start = 1,
               stop = str_locate(master_data[c(62,156,191,762,861,1080),"gradespan"],"School")[,"start"] - 2)

#remove white spaces from front to back of string
master_data[,"gradespan"] <- master_data[,"gradespan"] %>% str_trim(side = "both")

#replace strings with PK or 3K with just K
master_data[,"gradespan"] <- master_data[,"gradespan"] %>% str_replace("PK","K")
master_data[,"gradespan"] <- master_data[,"gradespan"] %>% str_replace("3K","K")

#want to create a new column "School Type"
master_data$School.Type <- NA

#Want to create 3 broad groups to encapsulate all of our gradespans for the map
Elementary <- c("K","1","2","3","4","5")
Middle <- c("6","7","8")

#10,11,12 will be represented by Ten,Eleven,Twelve so as to completely separate them
#from 1 and 2 (The condition (sum(str_detect("12", Elementary) ))) would be true because the Elementary
#has '1' and '2', even though what we really want is FALSE since it's not twelve.
High <- c("9","Ten","Eleven","Twelve")

#we will also temp replace 10,11,12 with the word representatives in our dataset until our loop is done
master_data[,"gradespan"] <- master_data[,"gradespan"] %>% str_replace("10","Ten")
master_data[,"gradespan"] <- master_data[,"gradespan"] %>% str_replace("11","Eleven")
master_data[,"gradespan"] <- master_data[,"gradespan"] %>% str_replace("12","Twelve")

#now we want to fill up our School Type column with values: Elementary School, Middle School,
#Elementary/Middle School, Middle/High School, All

for (i in 1:nrow(master_data)) {

        if ((sum(str_detect(master_data$gradespan[i], Elementary) >= 1))
            &(sum(str_detect(master_data$gradespan[i], High) >= 1))
        ){master_data$School.Type[i] <- "All"}

        else if ((sum(str_detect(master_data$gradespan[i], Middle) >= 1))
                 &(sum(str_detect(master_data$gradespan[i], High) >= 1)))
                {master_data$School.Type[i] <- "Middle/High"}

        else if ((sum(str_detect(master_data$gradespan[i], Elementary) >= 1))
                 &(sum(str_detect(master_data$gradespan[i], Middle) >= 1)))
                {master_data$School.Type[i] <- "Elementary/Middle"}

        else if (sum(str_detect(master_data$gradespan[i], Middle) >= 1))
                {master_data$School.Type[i] <- "Middle"}

        else if (sum(str_detect(master_data$gradespan[i], Elementary) >= 1))
                {master_data$School.Type[i] <- "Elementary"}



}

#now revert the change and make Ten, Eleven, Twelve into numbers
master_data[,"gradespan"] <- master_data[,"gradespan"] %>% str_replace("Ten","10")
master_data[,"gradespan"] <- master_data[,"gradespan"] %>% str_replace("Eleven","11")
master_data[,"gradespan"] <- master_data[,"gradespan"] %>% str_replace("Twelve","12")
