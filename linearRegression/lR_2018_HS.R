library(corrplot)
library(tidyverse)
library(Hmisc)
library(rstatix)
library(sjPlot)

#load in our data
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

#get new column : "Salary per classroom teacher")
df <- df %>% mutate(SPCT = Classroom.Teachers_2017.18
                    / (Classroom.Teachers.w..0.3.Years.Experience_2017.18 + Classroom.Teachers.w..More.than.3.Years.Experience_2017.18)
)

#rename long column names
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
#get rid of uneccessary columns
df <- df %>% select(-c(1,14:18))

#scaled the dataframe, then turned it back to a dataframe for linear regression analysis
df <- data.frame(scale(df))
#linear regression on all stem regents
mscca <- lm(MSCCA ~  Percent.Poverty + TSFPP + RTS + PSTA.17A + PSTA.17B + PSTA.17C + STEM_AP_Courses_Offered + club_number
            + SPCT , df)

mscca2 <- lm(MSCCA2 ~  Percent.Poverty + TSFPP + RTS + PSTA.17A + PSTA.17B + PSTA.17C + STEM_AP_Courses_Offered + club_number
             + SPCT , df)
msccg <- lm(MSCCG ~  Percent.Poverty + TSFPP + RTS + PSTA.17A + PSTA.17B + PSTA.17C + STEM_AP_Courses_Offered + club_number
            + SPCT , df)

msle <- lm(MSLE ~  Percent.Poverty + TSFPP + RTS + PSTA.17A + PSTA.17B + PSTA.17C + STEM_AP_Courses_Offered + club_number
           + SPCT , df)

mses <- lm(MSES ~  Percent.Poverty + TSFPP + RTS + PSTA.17A + PSTA.17B + PSTA.17C + STEM_AP_Courses_Offered + club_number
           + SPCT , df)

msc <- lm(MSC ~  Percent.Poverty + TSFPP + RTS + PSTA.17A + PSTA.17B + PSTA.17C + STEM_AP_Courses_Offered + club_number
          + SPCT , df)

msp <- lm(MSP ~  Percent.Poverty + TSFPP + RTS + PSTA.17A + PSTA.17B + PSTA.17C + STEM_AP_Courses_Offered + club_number
          + SPCT , df)

#showcase the results with tab_model (library(sjPlot))
#tab_model(mscca, mscca2, msccg, msle, mses, msc, msp)
