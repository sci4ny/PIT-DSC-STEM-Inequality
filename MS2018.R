#load packages
library(dplyr)
library(readxl)
library(writexl)
library(naniar)

#load directory
load <- read_excel("C:/Users/thena/OneDrive/Documents/STEM Research/Datasets/2018_DOE_Middle_School_Directory.xlsx")
directory <-load %>% select('schooldbn', 'printedschoolname', 'Borough', 'gradespan', 'Latitude', 'Longitude')
directory_save <- directory

#load CSR
csr_load <- read_excel("C:/Users/thena/OneDrive/Documents/STEM Research/Datasets/2017-2018_Computer_Science_Report_LL177.xlsx", sheet = 3, skip = 4)
csr_load2 <- read_excel("C:/Users/thena/OneDrive/Documents/STEM Research/Datasets/2017-2018_Computer_Science_Report_LL177.xlsx", sheet = 6, skip = 4)
csr_load <- csr_load %>% select(1,4,5,6,7) #ratio, student enrollment
csr_load2 <- csr_load2 %>% select(2,4) #bandwith
csr_load3 <- read_excel("C:/Users/thena/OneDrive/Documents/STEM Research/Datasets/2017-2018_Computer_Science_Report_LL177.xlsx", sheet = 2)
csr_load3 <- csr_load3 %>% select(1,3,4,5,6)

#load funding: K-12 Enrollment, K-12 FRPL Count, Classroom Teachers w and w/o more than 3 years experience, Total School Funding Per Pupil, and  Class Teachers
funding_load <- read_excel("C:/Users/thena/OneDrive/Documents/STEM Research/Datasets/2018-schools-parts-b-e (1).xlsx", sheet = "Sheet1")
funding_columns <- funding_load %>% select(5,14,17,20:21,28,49)

#load surveys: Pos & Neg % Q17
survey_load <- read_excel("C:/Users/thena/OneDrive/Documents/STEM Research/Datasets/2017_-_2018_NYC_School_Survey_Teacher_Data.xlsx", sheet = "Teacher Pos & Neg %")
survey_columns <- survey_load %>% select(1,3,173:178)
survey_columns <- survey_columns[-c(1,2),] #trim top 2
names(survey_columns) <- c('DBN', 'Teacher_Response_Rate', '17a. SD/D', '17a. SA/A', '17b. SD/D,', '17b. SA/A', '17c. SD/D', '17c. SA/A')

#load tests
math_test_load <- read_excel("C:/Users/thena/OneDrive/Documents/STEM Research/Datasets/school-math-results-2013-2019-(public) (1).xlsx", sheet = "All")
math_test_columns <- math_test_load %>% select(2,4,5,7,9,11,13,15)
math_test_columns <- filter(math_test_columns, math_test_columns$Year == '2018' & math_test_columns$Grade %in% c(6,7,8)) #filter by grade and year

#remove s values or convert to NA
math_test_columns <- math_test_columns %>% replace_with_na_all(condition = ~.x == 's')
#which(is.na(math_test_columns), arr.ind=TRUE)

#sum grades by school
names(math_test_columns) <- c('DBN', 'grade', 'year', 'number_tested', 'number_lvl_one', 'number_level_two', 'number_level_three', 'number_lever_four') # rename columns for group_by
math_test_sum <- math_test_columns %>% group_by(DBN) %>%
  summarize(number_tested = sum(as.numeric(number_tested), na.rm = TRUE),
            number_lvl_one = sum(as.numeric(number_lvl_one), na.rm = TRUE),
            number_level_two = sum(as.numeric(number_level_two), na.rm = TRUE),
            number_level_three = sum(as.numeric(number_level_three), na.rm = TRUE),
            number_lever_four = sum(as.numeric(number_lever_four), na.rm = TRUE)
            )

#load poverty, remove words/spaces/%
poverty_load <- read.csv("C:/Users/thena/OneDrive/Documents/STEM Research/Datasets/poverty.csv")
names(poverty_load)[38] <- 'poverty_percent'
poverty_load <- filter(poverty_load, poverty_load$Year == '2017-18')
poverty_columns <- poverty_load %>% select(1,38)
poverty_columns$poverty_percent<-gsub("Above ","",as.character(poverty_columns$poverty_percent))
poverty_columns$poverty_percent<-gsub("Below ","",as.character(poverty_columns$poverty_percent))
poverty_columns$poverty_percent<-gsub("%","",as.character(poverty_columns$poverty_percent))

#join directory and CSR
csr_columns <- left_join(csr_load, csr_load2, by = 'DBN')
csr_columns <- left_join(csr_columns, csr_load3, by = 'DBN')
directory <- left_join(directory, csr_columns, by = c('schooldbn' = 'DBN'))

#join funding data with local codes, replace local codes with original codes
trim <- sub('..', '', directory$schooldbn) #get local codes
directory$schooldbn <- trim
directory <- left_join(directory, funding_columns, by = c('schooldbn' = 'Local School Code'))
directory$schooldbn <- directory_save$schooldbn #replace original codes

#join -teacher survey, trim dbns to local codes
directory <- left_join(directory, survey_columns, by = c('schooldbn' = 'DBN'))

#join math test
directory <- left_join(directory, math_test_sum, by = c('schooldbn' = 'DBN'))

#join poverty
directory <- left_join(directory, poverty_columns, by = c('schooldbn' = 'DBN'))

#NA Cleanup
na_look_columns <- directory %>% select (7:ncol(directory))
complete_cases <- rowSums(is.na(na_look_columns)) != ncol(na_look_columns)
directory_complete <- directory[complete_cases,]

#save final dataframe to xlsx
write_xlsx(directory,"C:/Users/thena/OneDrive/Documents/STEM Research/Datasets/2018 MS Directory & Metrics.xlsx")
write_xlsx(directory_complete,"C:/Users/thena/OneDrive/Documents/STEM Research/Datasets/2018 MS Directory & Metrics (Complete).xlsx")
