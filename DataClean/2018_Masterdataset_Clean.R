#this is the data wrangling for the master dataset (middle/elementary school)
library(dplyr)
library(stringr)


#-------------------------------------------------------------------------------
#this is where we load our directories (and also select only the relevant columns)
middle_dir <- read.csv("~/DataClean/Datasets/MappingDataset/2018_DOE_Middle_School_Directory.csv") %>%
     select(c(2,3,"Borough","gradespan","Latitude","Longitude"))

elementary_dir <- read.csv("~/DataClean/Datasets/MappingDataset/2019_DOE_Kindergarten_Admissions_Guide.csv") %>%
     select(c(1:3,13:14))

#this is where we add a borough column to elementary based on DBN code
#we want to add a borough column to the elementary_dir
elementary_dir$Borough <- NA

#want to go through every row and based on dbn determine the which borough the
#school resides in
for (i in 1:nrow(elementary_dir)) {

     #depending on the letter of the DBN, we assign it the appropriate borough

     if (substr(elementary_dir$DBN[i],3,3) == "X") {elementary_dir$Borough[i] <- "BRONX"}
     else if (substr(elementary_dir$DBN[i],3,3) == "M") {elementary_dir$Borough[i] <- "MANHATTAN"}
     else if (substr(elementary_dir$DBN[i],3,3) == "K") {elementary_dir$Borough[i] <- "BROOKLYN"}
     else if (substr(elementary_dir$DBN[i],3,3) == "Q") {elementary_dir$Borough[i] <- "QUEENS"}
     else if (substr(elementary_dir$DBN[i],3,3) == "R") {elementary_dir$Borough[i] <- "STATEN IS"}


}

#now we reaarange the columns in elementary_dir so it mirrors middle_dir
elementary_dir <- elementary_dir[,c(1,2,6,3,4,5)]

#this is to remove GR from the middle_dir's gradespan column
middle_dir[,4] <- str_remove(middle_dir[,4], "GR")

#rename some of the middleschool_dir/elementary dir columns
middle_dir <- middle_dir %>% rename(dbn = "schooldbn", School.Name = "printedschoolname")
elementary_dir <- elementary_dir %>% rename(dbn = "DBN", gradespan = "Gradespan")

#here we join both datasets
master_data <- middle_dir %>% full_join(elementary_dir)

#remove duplicates from master dataset
master_data <-  master_data[!duplicated(master_data$dbn), ]


#-------------------------------------------------------------------------------
#Now that we have our essentials (dbn, schoolname, lat/long, borough), we now add
#the inequality/testing metric data

#load our testing/inequality data

math_test_scores <- read.csv("~/DataClean/Datasets/TestScores and Regents/Math_test_results_2013-2019.csv") %>%
     select(c(2,4:5,7:8,10,12,14,16,18))

School_Bandwidth <- read.csv("~/DataClean/Datasets/CS report/2018_School_Bandwidth.csv")
STEM_Teacher <- read.csv("~/DataClean/Datasets/CS report/2018_STEM_Teacher.csv")
Teacher_Survey <- read.csv("~/DataClean/Datasets/Public data File and Survey/2018_Teacher_Survey.csv")
poverty_data <- read.csv("~/DataClean/Datasets/Poverty Data/2019-20_Demographic_Snapshot_-_School.csv")
School_funding <- read.csv("~/DataClean/Datasets/School Funding/2018-school-funding.csv")

#-------------------------------------------------------------------------------
#Math Test Scores: We want to filter this data by year = 2018 and grade = All grades, and rename columns
#and lastly remove unwanted columns
math_test_scores <- math_test_scores %>% filter( Year == 2018 & Grade == "All Grades")
math_test_scores <- math_test_scores %>% rename(dbn = "DBN", '%Level1' = "X..Level.1.1",
                                                '%Level2' = "X..Level.2.1", '%Level3' = "X..Level.3.1",
                                                "%Level4" = "X..Level.4.1", '%Level3&4' = "X..Level.3.4.1",
                                                Mean.Scale.Math.Score = "Mean.Scale.Score")

math_test_scores <- math_test_scores %>% select(-c(2:3))

#change char columns to numeric
math_test_scores[,c(3:8)] <- sapply(math_test_scores[,c(3:8)], as.numeric)

#finally join the math scores to our master dataset
master_data <- master_data %>% left_join(math_test_scores, by = "dbn")

#-------------------------------------------------------------------------------
#school bandwidth: we want to remove/rename some columns and then join it with master data
School_Bandwidth <- School_Bandwidth %>% select(-c(1,3,5))
School_Bandwidth <- School_Bandwidth %>% rename(dbn = X,
                                                Mbps.Bandwidth = X.2)

School_Bandwidth <- School_Bandwidth[-c(1:4),]
School_Bandwidth[,2] <- as.numeric(School_Bandwidth[,2])

#join with master
master_data <- master_data %>% left_join(School_Bandwidth, by = "dbn")

#-------------------------------------------------------------------------------
#STEM Teacher: We take Ratio of teacher to students column and full/part time
#STEM teacher
STEM_Teacher <- STEM_Teacher %>% select(-c(2:4))
STEM_Teacher <- STEM_Teacher %>% rename(dbn = ï..New.York.City.Department.of.Education,
                                        Full.Time.Certified.STEM.Teachers = X.3,
                                        Part.Time.Certified.STEM.Teachers = X.4,
                                        Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students = X.5)
STEM_Teacher <- STEM_Teacher[-c(1:4),]

#turn columns into numeric
STEM_Teacher[,c(2:3)] <- sapply(STEM_Teacher[,c(2:3)], as.numeric)

#turn each value in the column into a character vector with two values (we split it by :)
STEM_Teacher$Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students<- strsplit(STEM_Teacher$Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students,
                                                                    split = ":")

#for each row, we change the value of the row by accessing the character vector
#and taking its first element [1] and dividing it by the second element [2]
#so that we convert the ratio into a decimal
for (p in 1:length(STEM_Teacher$Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students)){
     STEM_Teacher$Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students[p] <-
          as.numeric(STEM_Teacher$Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students[[p]])[1] /
          as.numeric(STEM_Teacher$Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students[[p]])[2]
}

STEM_Teacher$Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students <- as.numeric(STEM_Teacher$Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students)

#unlist so it can be converted properly to csv
STEM_Teacher$Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students <-
     unlist(STEM_Teacher$Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students)

#finally, join with master data
master_data <- master_data %>% left_join(STEM_Teacher, by = "dbn")


#-------------------------------------------------------------------------------
#Teacher Survey: want to take column that pertains to asking wether STEM teachers
#agree that they have adequate resources to teach a class
Teacher_Survey <- Teacher_Survey %>% select(1,3,173:178)

Teacher_Survey <- Teacher_Survey %>% rename(dbn = ï..DBN,
                                            Percent.of.STEMTeachers.That.Agree.With.17a =
                                                 X.87,
                                            Percent.of.STEMTeachers.That.Agree.With.17b =
                                                 X.88,
                                            Percent.of.STEMTeachers.That.Agree.With.17c =
                                                 X.89)

Teacher_Survey  <- Teacher_Survey[-c(1:2),]
Teacher_Survey <- Teacher_Survey %>% select(c(1:2,4,6,8))

#transform columns into numeric
Teacher_Survey[,3] <- str_replace(Teacher_Survey[,3], "%","")
Teacher_Survey[,4] <- str_replace(Teacher_Survey[,4], "%","")
Teacher_Survey[,5] <- str_replace(Teacher_Survey[,5], "%","")

Teacher_Survey[,c(3:5)] <- sapply(Teacher_Survey[,c(3:5)], as.numeric)

#divide by 100 to turn it into decimal
Teacher_Survey[,3:5] <- Teacher_Survey[,3:5] /100

#join with master data
master_data <- master_data %>%  left_join(Teacher_Survey, by = "dbn")



#-------------------------------------------------------------------------------
#Poverty_data: Want to filter and then take column that tells us the percentage of
#students in poverty column
poverty_data <- poverty_data %>% filter( Year == "2017-18")
poverty_data <- poverty_data %>% select(c(1,38))
poverty_data <- poverty_data %>% rename(dbn = DBN,
                                        Percent.Poverty = X..Poverty.1)
#want to make poverty column numeric
poverty_data$Percent.Poverty <- str_replace(poverty_data$Percent.Poverty, "Above 95%", "95")
poverty_data$Percent.Poverty <- str_replace(poverty_data$Percent.Poverty, "%", "")
poverty_data$Percent.Poverty <- as.numeric(poverty_data$Percent.Poverty)
poverty_data$Percent.Poverty <- poverty_data$Percent.Poverty / 100

#join with master data
master_data <- master_data %>% left_join(poverty_data, by = "dbn")

#-------------------------------------------------------------------------------
#school_funding: Want to filter by 2017-18 and take relevant columns
School_funding <- School_funding %>% filter(ï..School.Year == "2017-18")
School_funding <- School_funding %>% select(c(5,14,17,20:21,28,49))

#create temp column for the sake of merging by Local School Code
master_data$Local.School.Code <- substring(master_data$dbn,3,6)

#join with master data, then remove local.school.code column
master_data <- master_data %>% left_join(School_funding, by = "Local.School.Code")
master_data <- master_data %>% select(-c(23))

#turn columns into numeric values
master_data[,24:26] <- sapply(master_data[,24:26], as.numeric)
master_data[,23] <- str_remove(master_data[,23],",")
master_data[,23] <- as.numeric(master_data[,23])

master_data[,28] <- str_remove(master_data[,28],",")
master_data[,28] <- as.numeric(master_data[,28])

master_data[,27] <- str_remove_all(master_data[,27],",")
master_data[,27] <- as.numeric(master_data[,27])

#-------------------------------------------------------------------------------
#now we add column for salary per classroom teacher
master_data <- master_data %>% mutate(Salary.Per.Classroom.Teacher = Classroom.Teachers/
                                           (Classroom.Teachers.w..0.3.Years.Experience + Classroom.Teachers.w..More.than.3.Years.Experience))

#we arrange (sort) by dbn
master_data <- master_data %>% arrange(dbn )
