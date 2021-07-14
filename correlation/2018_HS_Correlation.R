library(corrplot)
library(tidyverse)
library(Hmisc)
library(rstatix)


#load in dataset and select relevant metrics
df <- read.csv("~/PIT-DSC-STEM-Inequality/data/2018_HS_Data.csv") %>% select(c("dbn","Mean.Score_Common.Core.Algebra","Mean.Score_Common.Core.Algebra2",
                                                                      "Mean.Score_Common.Core.Geometry","Mean.Score_Living.Environment",
                                                                      "Mean.Score_Physical.Settings.Earth.Science","Mean.Score_Physical.Settings.Chemistry",
                                                                      "Mean.Score_Physical.Settings.Physics","Mbps.Bandwidth", "Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students",
                                                                      "Percent.of.STEMTeachers.That.Agree.With.17a","Percent.of.STEMTeachers.That.Agree.With.17b",
                                                                      "Percent.of.STEMTeachers.That.Agree.With.17c","K.12.Enrollment_2017.18",
                                                                      "K.12.FRPL.Count_2017.18", "Classroom.Teachers.w..0.3.Years.Experience_2017.18",
                                                                      "Classroom.Teachers.w..More.than.3.Years.Experience_2017.18",
                                                                      "Classroom.Teachers_2017.18","Total.School.Funding.per.Pupil_2017.18",
                                                                      "Percent.Poverty","STEM_AP_Courses_Offered","club_number"))



#mutate column to get salary per teacher
df <- df %>% mutate(SPCT = Classroom.Teachers_2017.18
                    / (Classroom.Teachers.w..0.3.Years.Experience_2017.18 + Classroom.Teachers.w..More.than.3.Years.Experience_2017.18)
)
#rename long column names to shorter names
df <- df %>% rename( MSCCA = Mean.Score_Common.Core.Algebra,
                     MSCCA2 = Mean.Score_Common.Core.Algebra2,
                     MSCCG = Mean.Score_Common.Core.Geometry,
                     MSLE = Mean.Score_Living.Environment,
                     MSC = Mean.Score_Physical.Settings.Chemistry,
                     MSES = Mean.Score_Physical.Settings.Earth.Science,
                     MSP = Mean.Score_Physical.Settings.Physics,
                     PSTA.17A = Percent.of.STEMTeachers.That.Agree.With.17a,
                     PSTA.17B = Percent.of.STEMTeachers.That.Agree.With.17b,
                     PSTA.17C = Percent.of.STEMTeachers.That.Agree.With.17c,
                     TSFPP = Total.School.Funding.per.Pupil_2017.18,
                     RTS = Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students)

#remove unwated columns
df <- df %>% select(-c(1,14:18))

#want to turn data frame to matrix so that rcorr() works
M <- rcorr(as.matrix(df), type = "pearson")
#we want to plot the corr matrix, so we do M$r
cor_plot(M$r, method = "square")
