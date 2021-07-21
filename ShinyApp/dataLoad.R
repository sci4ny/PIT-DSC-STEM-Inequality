#This is for our global variables/data
library(dplyr)
library(MASS)
library(stringr)

#load hs data
hs_data <- read.csv("~/PIT-DSC-STEM-Inequality/data/2018_HS_Data.csv")
ms_df <- read_excel('~/PIT-DSC-STEM-Inequality/data/2018 MS Metrics (Complete).xlsx')
csr <- read_excel("~/PIT-DSC-STEM-Inequality/data/Computer Science Report.xlsx", sheet = "STEM Teachers", col_types = c("text", "skip", "text", "skip", "skip", "skip", "text"), skip = 3)

#turn decimal number in the ratio column into fractions, then turn fractions into ratios
hs_data[,31] <- paste(fractions(hs_data[,31], cycles = 3), " ")
hs_data[,31] <- str_replace(hs_data[,31],"/", ":")

ms_df <- ms_df %>% left_join(csr, by = 'DBN')
ms_df <- subset(ms_df, select = -c(9) )

#remove whitespace from end of string and front of string
hs_data[,"Borough"] <- hs_data[,"Borough"] %>%  str_trim(side = "both")

#adds salary per classroom column
hs_data <- hs_data %>% mutate('Salary Per Classroom Teacher' = Classroom.Teachers_2017.18
                    / (Classroom.Teachers.w..0.3.Years.Experience_2017.18 + Classroom.Teachers.w..More.than.3.Years.Experience_2017.18)
)
