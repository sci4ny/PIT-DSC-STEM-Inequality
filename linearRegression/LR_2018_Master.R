library(corrplot)
library(tidyverse)
library(Hmisc)
library(rstatix)
library(sjPlot)

#load in our data
df <- read.csv("~/PIT-DSC-STEM-Inequality/data/2018_Master_Data.csv") %>% select(c(9:15,18,20:23,29:30))

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

#scaled the dataframe, then turned it back to a dataframe for linear regression analysis
df <- data.frame(scale(df))

#linear regression on percent math scores and mean math scores
mathscore <- lm( Mean.Scale.Math.Score ~ Mbps.Bandwidth + RTS + PSTA.17a + PSTA.17b + PSTA.17c + Percent.Poverty
                 + TSFPP + SPCT, df)

percentOne <- lm(Percent.Level1 ~  Mbps.Bandwidth + RTS + PSTA.17a + PSTA.17b + PSTA.17c + Percent.Poverty
                 + TSFPP + SPCT, df)


percentTwo <- lm(Percent.Level2 ~  Mbps.Bandwidth + RTS + PSTA.17a + PSTA.17b + PSTA.17c + Percent.Poverty
                 + TSFPP + SPCT, df)

percentThree <- lm(Percent.Level3 ~  Mbps.Bandwidth + RTS + PSTA.17a + PSTA.17b + PSTA.17c + Percent.Poverty
                 + TSFPP + SPCT, df)

percentFour <- lm(Percent.Level4 ~  Mbps.Bandwidth + RTS + PSTA.17a + PSTA.17b + PSTA.17c + Percent.Poverty
                 + TSFPP + SPCT, df)
percentThreeFour <- lm(Percent.Level3and4 ~  Mbps.Bandwidth + RTS + PSTA.17a + PSTA.17b + PSTA.17c + Percent.Poverty
                 + TSFPP + SPCT, df)

#showcase the results with tab_model
#tab_model(mathscore, percentOne, percentTwo, percentThree, percentFour, percentThreeFour)
