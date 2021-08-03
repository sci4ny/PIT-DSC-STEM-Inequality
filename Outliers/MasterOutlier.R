#outlier dataset containing every school range
library(dplyr)
library(readxl)

#load data
outlier_hs <- read.csv("~/PIT-DSC-STEM-Inequality/Outliers/hsoutliers.csv") %>% select(-c(1))
outlier_ms <- read.csv("~/PIT-DSC-STEM-Inequality/Outliers/msoutliers.csv") %>% select(-c(1,8,10))
outlier_ele <- read_excel("~/PIT-DSC-STEM-Inequality/Outliers/elementaryoutliers.xlsx") %>% select(-c(7))

#load school datasets in particular to include the test scores for elementary and high school
hs <- read.csv("~/PIT-DSC-STEM-Inequality/data/2018_HS_Data.csv") %>% select(c(1,8,11,23))
elementary <- read_excel("~/PIT-DSC-STEM-Inequality/data/elementarymapdata.xlsx") %>% select(c(1,51)) %>% rename(dbn = DBN)

#rename columns
outlier_ms <- outlier_ms %>% rename(dbn = ï..DBN,
                                    school_name = School.Name,
                                    grade_span = School.Type,
                                    )

outlier_ele <- outlier_ele %>% rename(dbn = DBN,
                                      school_name = Name,
                                      grade_span = Gradespan)

#re-arrange columns
outlier_ms <- outlier_ms[,c(1,2,4,3,5,6,8,9,7)]
outlier_ele <- outlier_ele[,c(1,2,6,7,3,4,5,8)]
outlier_ele$Borough <- toupper(outlier_ele$Borough)

#update middleschool data outlier links
outlier_ms$Website[1] <- "https://sites.google.com/is223montauk.org/site/"
outlier_ms$Website[2] <- "http://www.davinci61.com/"
outlier_ms$Website[3] <- "https://www.casa311.org/"
outlier_ms$Website[4] <- "https://www.145innovators.com/"

#join the test score data with outlier data for elementary and high school
outlier_ele <- outlier_ele %>% left_join(elementary,by = 'dbn' )
outlier_hs <- outlier_hs %>% left_join(hs, by = "dbn")

outlier_ele <- outlier_ele %>% rename(Scaled.Mean.Score = MSS.All.Grades)


#join the datasets
outliers <- outlier_hs %>% full_join(outlier_ms)
outliers <- outliers %>% full_join(outlier_ele)

#save dataset
write.csv(outliers, file = "~/PIT-DSC-STEM-Inequality/data/Outliers.csv")
