#outliers of our dataset
#msle and percent poverty
library(dplyr)
library(here)


#load in our data
ms_data <- read.csv(here("data", "msOUtliers.csv"))


#mean/sd calculations for STEM tests
mean_Math.Proficiency <- mean(ms_data$Math.Proficiency, na.rm = TRUE) %>% round(digits = 2)
sd_Math.Proficiency <-  sd(ms_data$Math.Proficiency, na.rm = TRUE) %>% round(digits = 2)

mean_X..of.Comp.Sci.Courses <- mean(ms_data$X..of.Comp.Sci.Courses, na.rm = TRUE) %>% round(digits = 2)
sd_X..of.Comp.Sci.Courses <-  sd(ms_data$X..of.Comp.Sci.Courses, na.rm = TRUE) %>% round(digits = 2)

mean_Teacher.Student.Ratio <- mean(ms_data$Teacher.Student.Ratio, na.rm = TRUE) %>% round(digits = 2)
sd_Teacher.Student.Ratio <-  sd(ms_data$Teacher.Student.Ratio, na.rm = TRUE) %>% round(digits = 2)

mean_Mbps.Bandwidth <- mean(ms_data$Mbps.Bandwidth, na.rm = TRUE) %>% round(digits = 2)
sd_Mbps.Bandwidth <-  sd(ms_data$Mbps.Bandwidth, na.rm = TRUE) %>% round(digits = 2)

mean_PD17a <- mean(ms_data$PD17a, na.rm = TRUE) %>% round(digits = 2)
sd_PD17a <-  sd(ms_data$PD17a, na.rm = TRUE) %>% round(digits = 2)

mean_PD17b <- mean(ms_data$PD17b, na.rm = TRUE) %>% round(digits = 2)
sd_PD17b <-  sd(ms_data$PD17b, na.rm = TRUE) %>% round(digits = 2)

mean_PD17c <- mean(ms_data$PD17c, na.rm = TRUE) %>% round(digits = 2)
sd_PD17c <-  sd(ms_data$PD17c, na.rm = TRUE) %>% round(digits = 2)

mean_Scaled.Mean.Score <- mean(ms_data$Scaled.Mean.Score, na.rm = TRUE) %>% round(digits = 2)
sd_Scaled.Mean.Score <- sd(ms_data$Scaled.Mean.Score, na.rm = TRUE) %>% round(digits = 2)

mean_FRPL <- mean(ms_data$FRPL, na.rm = TRUE) %>% round(digits = 2)
sd_FRPL <-  sd(ms_data$FRPL, na.rm = TRUE) %>% round(digits = 2)

mean_TSFPP <- mean(ms_data$TSFPP, na.rm = TRUE) %>% round(digits = 2)
sd_TSFPP <-  sd(ms_data$TSFPP, na.rm = TRUE) %>% round(digits = 2)

mean_Average.Teacher.Salary <- mean(ms_data$Average.Teacher.Salary, na.rm = TRUE) %>% round(digits = 2)
sd_Average.Teacher.Salary <-  sd(ms_data$Average.Teacher.Salary, na.rm = TRUE) %>% round(digits = 2)

mean_PLVL34 <- mean(ms_data$PD17c, na.rm = TRUE) %>% round(digits = 2)
sd_PLVL34 <-  sd(ms_data$PD17c, na.rm = TRUE) %>% round(digits = 2)

mean_Stem.Activities.Count <- mean(ms_data$Stem.Activities.Count, na.rm = TRUE) %>% round(digits = 2)
sd_Stem.Activities.Count <-  sd(ms_data$Stem.Activities.Count, na.rm = TRUE) %>% round(digits = 2)


#mean/sd for poverty
mean_Poverty.Percent <- mean(ms_data$Poverty.Percent, na.rm = TRUE) %>% round(digits = 2)
sd_Poverty.Percent <- sd(ms_data$Poverty.Percent, na.rm = TRUE) %>% round(digits = 2)


#1 sd above the mean for STEM tests
test_Math.Proficiency <- mean_Math.Proficiency + 1*sd_Math.Proficiency
test_X..of.Comp.Sci.Courses <- mean_X..of.Comp.Sci.Courses + 1*sd_X..of.Comp.Sci.Courses
test_Teacher.Student.Ratio <- mean_Teacher.Student.Ratio + 1*sd_Teacher.Student.Ratio
test_Mbps.Bandwidth <- mean_Mbps.Bandwidth + 1*sd_Mbps.Bandwidth
test_PD17a <- mean_PD17a + 1*sd_PD17a
test_PD17b <- mean_PD17b + 1*sd_PD17b
test_PD17c <- mean_PD17c + 1*sd_PD17c
test_Scaled.Mean.Score <- mean_Scaled.Mean.Score + 1*sd_Scaled.Mean.Score
test_FRPL <- mean_FRPL + 1*sd_FRPL
test_TSFPP <- mean_TSFPP + 1*sd_TSFPP
test_Average.Teacher.Salary <- mean_Average.Teacher.Salary + 1*sd_Average.Teacher.Salary
test_PLVL34 <- mean_PLVL34 + 1*sd_PLVL34
test_Stem.Activities.Count <- mean_Stem.Activities.Count + 1*sd_Stem.Activities.Count


#1 sd above the mean for poverty
test_Poverty.Percent <- mean_Poverty.Percent + .865*sd_Poverty.Percent


#filter to get the outliers that we want
outlier_Math.Proficiency <- ms_data %>% filter(Poverty.Percent >= test_Poverty.Percent & Math.Proficiency >= test_Math.Proficiency)
outlier_X..of.Comp.Sci.Courses <- ms_data %>% filter(Poverty.Percent >= test_Poverty.Percent & X..of.Comp.Sci.Courses >= test_X..of.Comp.Sci.Courses)
outlier_Teacher.Student.Ratio <- ms_data %>% filter(Poverty.Percent >= test_Poverty.Percent & Teacher.Student.Ratio >= test_Teacher.Student.Ratio)
outlier_Mbps.Bandwidth <- ms_data %>% filter(Poverty.Percent >= test_Poverty.Percent & Mbps.Bandwidth >= test_Mbps.Bandwidth)
outlier_PD17a <- ms_data %>% filter(Poverty.Percent >= test_Poverty.Percent & PD17a >= test_PD17a)
outlier_PD17b <- ms_data %>% filter(Poverty.Percent >= test_Poverty.Percent & PD17b >= test_PD17b)
outlier_PD17c <- ms_data %>% filter(Poverty.Percent >= test_Poverty.Percent & PD17c >= test_PD17c)
outlier_Scaled.Mean.Score <- ms_data %>% filter(Poverty.Percent >= test_Poverty.Percent & Scaled.Mean.Score >= test_Scaled.Mean.Score)
outlier_FRPL <- ms_data %>% filter(Poverty.Percent >= test_Poverty.Percent & FRPL >= test_FRPL)
outlier_TSFPP <- ms_data %>% filter(Poverty.Percent >= test_Poverty.Percent & TSFPP >= test_TSFPP)
outlier_Average.Teacher.Salary <- ms_data %>% filter(Poverty.Percent >= test_Poverty.Percent & Average.Teacher.Salary >= test_Average.Teacher.Salary)
outlier_PLVL34 <- ms_data %>% filter(Poverty.Percent >= test_Poverty.Percent & PLVL34 >= test_PLVL34)
outlier_Stem.Activities.Count <- ms_data %>% filter(Poverty.Percent >= test_Poverty.Percent & Stem.Activities.Count >= test_Stem.Activities.Count)

#create a dataset with all the outlier data with schools
#outlier_data1 <- outlier_Chemistry %>% full_join(outlier_Algebra)
#outlier_data <- outlier_data1 %>%  full_join(outlier_Algebra2)
outlier_data <- outlier_Math.Proficiency %>% full_join(outlier_Scaled.Mean.Score) %>% full_join(outlier_PLVL34)
write.csv(outlier_data, file = "C:/Users/thena/Google Drive/STEM Research/STEM Inequality R Repository/Outliers/msoutliers.csv")
