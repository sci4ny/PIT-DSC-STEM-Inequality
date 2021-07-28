library(corrplot)
library(tidyverse)
library(Hmisc)
library(rstatix)


#load dataset
df <- read.csv("~/PIT-DSC-STEM-Inequality/data/2018_Master_Data.csv") %>%
     select(c(9:15,18,20:23,29:30))

#rename columns
df <- df %>% rename(RTS = Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students,
                    PSTA.17a = Percent.of.STEMTeachers.That.Agree.With.17a,
                    PSTA.17b = Percent.of.STEMTeachers.That.Agree.With.17b,
                    PSTA.17c = Percent.of.STEMTeachers.That.Agree.With.17c,
                    TSFPP = Total.School.Funding.per.Pupil,
                    SPCT = Salary.Per.Classroom.Teacher,
                    Percent.Level1 = X.Level1,
                    Percent.Level2 = X.Level2,
                    Percent.Level3 = X.Level3,
                    Percent.Level4 = X.Level4,
                    Percent.Level3and4 = X.Level3.4)

#want to turn data frame to matrix so that rcorr() works
M <- rcorr(as.matrix(df), type = "pearson")

#we want to plot the corr matrix, so we do M$r
cor_plot(M$r, method = "square")
