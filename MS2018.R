#load packages
library(dplyr)
library(readxl)
library(writexl)

#load directory
load <- read_excel("C:/Users/thena/OneDrive/Documents/STEM Research/Datasets/2018_DOE_Middle_School_Directory.xlsx")
directory <-load %>% select('schooldbn', 'printedschoolname', 'Borough', 'gradespan', 'Latitude', 'Longitude')
directory_save <- directory

#load CSR
csr_load <- read_excel("C:/Users/thena/OneDrive/Documents/STEM Research/Datasets/2017-2018_Computer_Science_Report_LL177.xlsx", sheet = 3, skip = 4)
csr_load2 <- read_excel("C:/Users/thena/OneDrive/Documents/STEM Research/Datasets/2017-2018_Computer_Science_Report_LL177.xlsx", sheet = 6, skip = 4)
csr_load <- csr_load %>% select(1,4,7) #ratio, student enrollment
csr_load2 <- csr_load2 %>% select(2,4) #bandwith

#load funding: K-12 Enrollment, K-12 FRPL Count, Classroom Teachers w and w/o more than 3 years experience, Total School Funding Per Pupil, and  Class Teachers
funding_load <- read_excel("C:/Users/thena/OneDrive/Documents/STEM Research/Datasets/2018-schools-parts-b-e (1).xlsx", sheet = "Sheet1")
funding_columns <- funding_load %>% select(5,14,17,20:21,28,49)

#load surveys: Pos & Neg % Q17
survey_load <- read_excel("C:/Users/thena/OneDrive/Documents/STEM Research/Datasets/2017_-_2018_NYC_School_Survey_Teacher_Data.xlsx", sheet = "Teacher Pos & Neg %")
survey_columns <- survey_load %>% select(1,173:178)
survey_columns <- survey_columns[-c(1,2),] #trim top 2

#load tests
math_test_load <- read_excel("C:/Users/thena/OneDrive/Documents/STEM Research/Datasets/school-math-results-2013-2019-(public) (1).xlsx", sheet = "All")
math_test_columns <- math_test_load %>% select(2,4,5,7,9,11,13,15)
math_test_columns <- filter(math_test_columns, math_test_columns$Year == '2018' & math_test_columns$Grade %in% c(6,7,8)) #filter by grade and year
names(math_test_columns) <- c('DBN', 'grade', 'year', 'number_tested', 'number_lvl_one', 'number_level_two', 'number_level_three', 'number_lever_four') # rename columns for group_by
math_test_sum <- math_test_columns %>% group_by(DBN) %>% #sum columns of interest by school (6->8)
  summarize(number_tested = sum(as.numeric(number_tested)),
            number_lvl_one = sum(as.numeric(number_lvl_one)),
            number_level_two = sum(as.numeric(number_level_two)),
            number_level_three = sum(as.numeric(number_level_three)),
            number_lever_four = sum(as.numeric(number_lever_four))
            )

#load poverty
poverty_load <- read.csv("C:/Users/thena/OneDrive/Documents/STEM Research/Datasets/poverty.csv")
names(poverty_load)[38] <- 'poverty_percent'
poverty_load <- filter(poverty_load, poverty_load$Year == '2017-18')
poverty_columns <- poverty_load %>% select(1,38)

#join directory and CSR
csr_columns <- left_join(csr_load, csr_load2, by = 'DBN')
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

#save final dataframe to xlsx
write_xlsx(directory,"C:/Users/thena/OneDrive/Documents/STEM Research/Datasets/2018 MS Directory & Metrics.xlsx")
