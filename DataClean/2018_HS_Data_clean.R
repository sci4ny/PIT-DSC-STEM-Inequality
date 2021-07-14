library(tidyverse)
library(dplyr)

#load in our Datasets
HS_data_2019 <- read.csv("~/DataClean/mapping/2019_DOE_High_School_Directory.csv")
HS_data_2018 <- read.csv("~/DataClean/mapping/2018_DOE_High_School_Directory.csv")
missingmapping <- read.csv("missingrowsfor2018.csv")
Regent_results <- read.csv("~/Dataclean/Datasets/TestScores and Regents/NYC_Regents.csv")
School_Bandwidth <- read.csv("~/DataClean/Datasets/CS report/2018_School_Bandwidth.csv")
STEM_Teacher <- read.csv("~/DataClean/Datasets/CS report/2018_STEM_Teacher.csv")
CS_Programs <- read.csv ("~/DataClean/Datasets/CS report/2018_CS_Programs.csv")
Teacher_Survey <- read.csv("~/DataClean/Datasets/Public data File and Survey/2018_Teacher_Survey.csv")
School_funding <- read.csv("~/DataClean/Datasets/School Funding/2018_School_Funding_Data.csv")
poverty_data <- read.csv("~/DataClean/Datasets/Poverty Data/2019-20_Demographic_Snapshot_-_School.csv")


#filter by 2018 and school level for HS data we filter by relevant data we want to keep
Regent_results <- Regent_results %>% filter(Year == 2018 , School.Level == "High school")

#we want to have updated lat/long for our HS 2018 dataset, so we take the lat/long
#of the 2019 dataset and the dbn so we can join with 2018 dataset
HS_data_2019 <- HS_data_2019 %>% select(1,455,456)

#take the relevant metrics we want from the 2018 DOE HS dataset
HS_data_2018 <- HS_data_2018 %>% select(1,2,"Borough","grades2018",455,456)

#join 2019 and 2018 by dbn
HS_data_2018 <- HS_data_2018 %>% left_join(HS_data_2019, by = "dbn")

#remove rows for which lat/long for 2019 is not available
HS_data_2018 <- HS_data_2018[-c(1,33,40,83,144,172,185,232,407),]

#remove the outdated lat/long columns, and rename those columns
HS_data_2018 <- HS_data_2018 %>% select(-c(5,6))
HS_data_2018 <- HS_data_2018 %>% rename(Latitude = Latitude.y, Longitude = Longitude.y)

#missingmapping contains the schools for which there was no updated 2019 lat/long
#we strip X column and location column from the missingmapping data and join with the 2018 data
missingmapping <- missingmapping %>% select(-c(1,"location"))
HS_data_2018 <- HS_data_2018 %>% full_join(missingmapping)

#remove unwanted columns; rename columns ; remove rows 1-4
School_Bandwidth <- School_Bandwidth %>% select(-c(1,3,5))
School_Bandwidth <- School_Bandwidth %>% rename(dbn = X,
                                                Mbps.Bandwidth = X.2)
School_Bandwidth <- School_Bandwidth[-c(1:4),]

#remove unwanted columns, remove/rename rows for STEM_Teacher dataset
STEM_Teacher <- STEM_Teacher %>% select(-c(2:4))
STEM_Teacher <- STEM_Teacher %>% rename(dbn = ï..New.York.City.Department.of.Education,
                                        Full.Time.Certified.STEM.Teachers = X.3,
                                        Part.Time.Certified.STEM.Teachers = X.4,
                                        Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students = X.5)
STEM_Teacher <- STEM_Teacher[-c(1:4),]

#remove/rename CS_programs dataset
CS_Programs <- CS_Programs %>% select(-c(2,7))
CS_Programs <- CS_Programs %>% rename(dbn = ï..DBN,
                                      Number.of.CS.Courses = X..of.Comp.Sci.Courses,
                                      Number.of.AP.CS.Courses = X..of.AP.Comp.Sci.Courses,
                                      number.of.Full.CS.Courses = X..of.Full.CS.Courses,
                                      number.of.Partial.CS.Courses = X..of.Partial.CS.Courses)

#rename/remove columns for teacher survey; remove rows
Teacher_Survey <- Teacher_Survey %>% select(1,3,173:178)

Teacher_Survey <- Teacher_Survey %>% rename(dbn = ï..DBN,
                                            Percent.of.STEMTeachers.That.Disagreed.With...In.planning.my.last.instructional.unit.I.had.the.resources.and.tools.I.needed.to.include.multiple.opportunities.for.focusing.deeply.on.the.concepts.emphasized.in.the.standards.to.help.students.build.strong.foundations.for.learning =
                                                 X17a...For.general.self.contained.math.science...In.planning.my.last.instructional.unit..I.had.the.resources.and.tools.I.needed.to.include.multiple.opportunities.for.focusing.deeply.on.the.concepts.emphasized.in.the.standards.to.help.students.build.strong.foundations.for.learning. ,
                                            Percent.of.STEMTeachers.That.Agreed.With...In.planning.my.last.instructional.unit.I.had.the.resources.and.tools.I.needed.to.include.multiple.opportunities.for.focusing.deeply.on.the.concepts.emphasized.in.the.standards.to.help.students.build.strong.foundations.for.learning =
                                                 X.87,
                                            Percent.of.STEMTeachers.That.Disagreed.With...In.planning.my.last.instructional.unit..I.had.the.resources.and.tools.I.needed.to.include.multiple.opportunities.for.creating.coherent.progressions.within.the.standards.from.previous.grades.to.current.grade.so.student.knowledge.skills.build.onto.previous.learning.as.foundations.for.math.concepts =
                                                 X17b...For.general.self.contained.math.science...In.planning.my.last.instructional.unit..I.had.the.resources.and.tools.I.needed.to.include.multiple.opportunities.for.creating.coherent.progressions.within.the.standards.from.previous.grades.to.current.grade.so.student.knowledge.skills.build.onto.previous.learning.as.foundations.for.math.concepts. ,
                                            Percent.Of.STEMTeachers.That.Agreed.With...In.planning.my.last.instructional.unit..I.had.the.resources.and.tools.I.needed.to.include.multiple.opportunities.for.creating.coherent.progressions.within.the.standards.from.previous.grades.to.current.grade.so.student.knowledge.skills.build.onto.previous.learning.as.foundations.for.math.concepts =
                                                 X.88,
                                            Percent.of.STEMTEachers.That.Disagreed.With...In.planning.my.last.instructional.unit..I.had.the.resources.and.tools.I.needed.to.include.multiple.opportunities.for.developing.studentsâ...conceptual.understanding..procedural.fluency..and.their.ability.to.apply.math.in.context =
                                                 X17c...For.general.self.contained.math.science...In.planning.my.last.instructional.unit..I.had.the.resources.and.tools.I.needed.to.include.multiple.opportunities.for.developing.studentsâ...conceptual.understanding..procedural.fluency..and.their.ability.to.apply.math.in.context. ,
                                            Percent.of.STEMTeachers.That.Agree.With...In.planning.my.last.instructional.unit..I.had.the.resources.and.tools.I.needed.to.include.multiple.opportunities.for.developing.studentsâ...conceptual.understanding..procedural.fluency..and.their.ability.to.apply.math.in.context =
                                                 X.89)

Teacher_Survey  <- Teacher_Survey[-c(1:2),]

#remove col 1 and local.school.code column from School_funding
School_funding <- School_funding %>% select(-c(1:3))

#remove non-stem rows for regents data
Regent_results <- Regent_results[Regent_results$Regents.Exam == "Geometry" |  Regent_results$Regents.Exam == "Integrated Algebra" | Regent_results$Regents.Exam == "Physical Settings/Chemistry" |
                                      Regent_results$Regents.Exam == "Physical Settings/Physics" | Regent_results$Regents.Exam == "Physical Settings/Earth Science"
                                 | Regent_results$Regents.Exam == "Living Environment" | Regent_results$Regents.Exam == "Common Core Geometry"
                                 | Regent_results$Regents.Exam =="Common Core Algebra" | Regent_results$Regents.Exam == "Common Core Algebra2",]

#drop unwanted columns
Regent_results <- Regent_results %>% select(-c(1,11:18))

#turn every regent into columns and remove unwanted columns
Regent_results_wider <- Regent_results %>% pivot_wider(names_from = Regents.Exam, values_from = c(Total.Tested, Mean.Score))
Regent_results <- Regent_results_wider
Regent_results <- Regent_results %>% select(-c(14,15,23,24,"School.Type",
                                               "School.Level","Category"))

#take wanted columns from poverty dataset,filter by year 2017-2018, and rename
poverty_data <- poverty_data %>% filter( Year == "2017-18")
poverty_data <- poverty_data %>% select(c(1,38))
poverty_data <- poverty_data %>% rename(dbn = DBN,
                                        Percent.Poverty = X..Poverty.1)



#joining regent data with all HS regardless of range
hs <- HS_data_2018 %>% left_join(Regent_results, by = c("dbn" = "School.DBN"))
#remove unwanted columns
hs <- hs %>% select(-c("School.Name","Year"))
#rearrange order of columns
hs <- hs[,c(1:7,14,8,15,9,16,10,17,11,18,12,19,13,20)]

#also join data with mbps/STEM/CS program/Teacher Survey/School_funding data
hs <- hs %>% left_join(School_Bandwidth, by = 'dbn')
hs <- hs %>% left_join(STEM_Teacher, by = 'dbn')
hs <- hs %>% left_join(CS_Programs, by = "dbn")
hs <- hs %>% left_join(Teacher_Survey, by = "dbn")
hs <- hs %>% left_join(School_funding, by = "dbn")


#add column named "School_Type" and all the values are High School
hs$School_Type <- "High School"

#also join poverty data to our dataset
hs <- hs %>% left_join(poverty_data, by = 'dbn')

#remove unwanted column and rename columns.
hs <- hs %>% select(-c(30,32,34))
hs <- hs %>% rename(Percent.of.STEMTeachers.That.Agree.With.17a = Percent.of.STEMTeachers.That.Agreed.With...In.planning.my.last.instructional.unit.I.had.the.resources.and.tools.I.needed.to.include.multiple.opportunities.for.focusing.deeply.on.the.concepts.emphasized.in.the.standards.to.help.students.build.strong.foundations.for.learning,
                    Percent.of.STEMTeachers.That.Agree.With.17b = Percent.Of.STEMTeachers.That.Agreed.With...In.planning.my.last.instructional.unit..I.had.the.resources.and.tools.I.needed.to.include.multiple.opportunities.for.creating.coherent.progressions.within.the.standards.from.previous.grades.to.current.grade.so.student.knowledge.skills.build.onto.previous.learning.as.foundations.for.math.concepts,
                    Percent.of.STEMTeachers.That.Agree.With.17c = Percent.of.STEMTeachers.That.Agree.With...In.planning.my.last.instructional.unit..I.had.the.resources.and.tools.I.needed.to.include.multiple.opportunities.for.developing.studentsâ...conceptual.understanding..procedural.fluency..and.their.ability.to.apply.math.in.context,
                    grade_span = grades2018)

#arrange dataset based on dbn
hs <- hs %>% arrange(dbn)

#run script that has variable 'ap_data' which contains #of STEM courses offered
source("Cleaning_HS_AP_courses.R")

#join ap_data with our dataset
hs <- hs %>% left_join(ap_data, by = 'dbn')

###
###Mutate columns to tell us percentage of the overall population of students who took the test
###
hs <- hs %>% mutate(Percent.Students.Took.The.Test.For.CCAlgebra =  `Total.Tested_Common Core Algebra` / as.numeric(K.12.Enrollment_2017.18))
hs <- hs %>% mutate(Percent.Students.Took.The.Test.For.CCAlgebra2 =  `Total.Tested_Common Core Algebra2` / as.numeric(K.12.Enrollment_2017.18))
hs <- hs %>% mutate(Percent.Students.Took.The.Test.For.CCGeometry =  `Total.Tested_Common Core Geometry` / as.numeric(K.12.Enrollment_2017.18) )
hs <- hs %>% mutate(Percent.Students.Took.The.Test.For.LE = `Total.Tested_Living Environment` / as.numeric(K.12.Enrollment_2017.18))
hs <- hs %>% mutate(Percent.Students.Took.The.Test.For.ES =  `Total.Tested_Physical Settings/Earth Science` / as.numeric(K.12.Enrollment_2017.18))
hs <- hs %>% mutate(Percent.Students.Took.The.Test.For.Chemistry =  `Total.Tested_Physical Settings/Chemistry` / as.numeric(K.12.Enrollment_2017.18))
hs <- hs %>% mutate(Percent.Students.Took.The.Test.For.Physics = `Total.Tested_Physical Settings/Physics`/ as.numeric(K.12.Enrollment_2017.18))

#we rearrange the columns
hs <- hs[,c(1:8,42,9:10,43,11:12,44,13:14,45,15:16,46,17:18,47,19:20,48,21:41)]

###
###turns some columns as numeric
###

#turn mean score columns to numeric values
hs[,c(8,11,14,17,20,23,26)] <- sapply(hs[,c(8,11,14,17,20,23,26)], as.numeric)


#replace % sign with empty space so that we're able to turn it into a numeric value
hs$Percent.of.STEMTeachers.That.Agree.With.17a <- str_replace_all(hs$Percent.of.STEMTeachers.That.Agree.With.17a, "%","")
hs$Percent.of.STEMTeachers.That.Agree.With.17b <- str_replace_all(hs$Percent.of.STEMTeachers.That.Agree.With.17b, "%","")
hs$Percent.of.STEMTeachers.That.Agree.With.17c <- str_replace_all(hs$Percent.of.STEMTeachers.That.Agree.With.17c, "%","")

#turn it into numeric value
hs[,37:39] <- sapply(hs[,37:39], as.numeric)

#divide by 100 so we get the percentage as a decimal
hs[,37:39] <- hs[,37:39] / 100

#function that replace all commas with empty space so that we can turn our char value to
#numeric
rwc <- function(x) {
     str_replace_all(x, ",","")
}

#k12, classroom teacher,total school funding, and bandwidth all turn to numeric value
hs[,c(41,44,45)] <- hs[,c(41,44,45)] %>% sapply(rwc)
hs[,c(28,40:45)] <- hs[,c(28,40:45)] %>% sapply(as.numeric)

#turn character into numeric, first replace % sign in order to apply as.numeric()
#then turn into decimal by dividing by 100
hs$Total.Teacher.Response.Rate <- str_replace(hs$Total.Teacher.Response.Rate, "%", "")
hs$Total.Teacher.Response.Rate <- as.numeric(hs$Total.Teacher.Response.Rate)
hs$Total.Teacher.Response.Rate <- hs$Total.Teacher.Response.Rate / 100

#do it also to Percent.Poverty Column
hs$Percent.Poverty <- str_replace(hs$Percent.Poverty, "Above 95%", "")
hs$Percent.Poverty <- str_replace(hs$Percent.Poverty, "%", "")
hs$Percent.Poverty <- as.numeric(hs$Percent.Poverty)
hs$Percent.Poverty <- hs$Percent.Poverty / 100

###
###Turn ratio to decimal (for analysis purposes)
###ctrl + shift + c to multi-line comment the code if we want just ratio

#turn each value in the column into a character vector with two values (we split it by :)
hs$Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students<- strsplit(hs$Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students,
                                                                    split = ":")
#for each row, we change the value of the row by accessing the character vector
#and taking its first element [1] and dividing it by the second element [2]
#so that we convert the ratio into a decimal
for (p in 1:length(hs$Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students)){
     hs$Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students[p] <-
          as.numeric(hs$Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students[[p]])[1] /
          as.numeric(hs$Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students[[p]])[2]
}

hs$Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students <- as.numeric(hs$Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students)

#unlist so it can be converted properly to csv
# hs$Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students <-
#      unlist(hs$Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students)
