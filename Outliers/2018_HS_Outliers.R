#outliers of our dataset
#msle and percent poverty
library(dplyr)

#load in our data
hs_data <- read.csv("~/outliers/data/2018_HS_Data.csv")


#mean/sd calculations for STEM tests

hs_mean_test_Algebra <- mean(hs_data$Mean.Score_Common.Core.Algebra, na.rm = TRUE) %>% round(digits = 2)
hs_sd_test_Algebra <-  sd(hs_data$Mean.Score_Common.Core.Algebra, na.rm = TRUE) %>% round(digits = 2)

hs_mean_test_Algebra2 <- mean(hs_data$Mean.Score_Common.Core.Algebra2, na.rm = TRUE) %>% round(digits = 2)
hs_sd_test_Algebra2 <-  sd(hs_data$Mean.Score_Common.Core.Algebra2, na.rm = TRUE) %>% round(digits = 2)

hs_mean_test_Geometry <- mean(hs_data$Mean.Score_Common.Core.Geometry, na.rm = TRUE) %>% round(digits = 2)
hs_sd_test_Geometry <-  sd(hs_data$Mean.Score_Common.Core.Geometry, na.rm = TRUE) %>% round(digits = 2)


hs_mean_test_LE <- mean(hs_data$Mean.Score_Living.Environment, na.rm = TRUE) %>% round(digits = 2)
hs_sd_test_LE <-  sd(hs_data$Mean.Score_Living.Environment, na.rm = TRUE) %>% round(digits = 2)

hs_mean_test_ES <- mean(hs_data$Mean.Score_Physical.Settings.Earth.Science, na.rm = TRUE) %>% round(digits = 2)
hs_sd_test_ES <-  sd(hs_data$Mean.Score_Physical.Settings.Earth.Science, na.rm = TRUE) %>% round(digits = 2)

hs_mean_test_Chemistry <- mean(hs_data$Mean.Score_Physical.Settings.Chemistry, na.rm = TRUE) %>% round(digits = 2)
hs_sd_test_Chemistry <-  sd(hs_data$Mean.Score_Physical.Settings.Chemistry, na.rm = TRUE) %>% round(digits = 2)

hs_mean_test_Physics <- mean(hs_data$Mean.Score_Physical.Settings.Physics, na.rm = TRUE) %>% round(digits = 2)
hs_sd_test_Physics <-  sd(hs_data$Mean.Score_Physical.Settings.Physics, na.rm = TRUE) %>% round(digits = 2)



#mean/sd for poverty
hs_mean_pov <- mean(hs_data$Percent.Poverty, na.rm = TRUE) %>% round(digits = 2)
hs_sd_pov <- sd(hs_data$Percent.Poverty, na.rm = TRUE) %>% round(digits = 2)


#1 sd above the mean for STEM tests
one_sd_test_Algebra <- hs_mean_test_Algebra + 1*hs_sd_test_Algebra
one_sd_test_Algebra2 <- hs_mean_test_Algebra2 + 1*hs_sd_test_Algebra2
one_sd_test_Geometry <- hs_mean_test_Geometry + 1*hs_sd_test_Geometry
one_sd_test_LE <- hs_mean_test_LE + 1*hs_sd_test_LE
one_sd_test_ES <- hs_mean_test_ES + 1*hs_sd_test_ES
one_sd_test_Chemistry <- hs_mean_test_Chemistry + 1*hs_sd_test_Chemistry
one_sd_test_Physics <- hs_mean_test_Physics + 1*hs_sd_test_Physics


#1 sd above the mean for poverty
one_sd_pov_hs<- hs_mean_pov + 1*hs_sd_pov


#filter to get the outliers that we want
outlier_hs_Algebra <- hs_data %>% filter(Percent.Poverty >= one_sd_pov_hs & Mean.Score_Common.Core.Algebra >= one_sd_test_Algebra)
outlier_hs_Algebra2 <- hs_data %>% filter(Percent.Poverty >= one_sd_pov_hs & Mean.Score_Common.Core.Algebra2 >= one_sd_test_Algebra2)
outlier_hs_Geometry <- hs_data %>% filter(Percent.Poverty >= one_sd_pov_hs & Mean.Score_Common.Core.Geometry >= one_sd_test_Geometry)
outlier_hs_LE <- hs_data %>% filter(Percent.Poverty >= one_sd_pov_hs & Mean.Score_Living.Environment >= one_sd_test_LE)
outlier_ES <- hs_data %>% filter(Percent.Poverty >= one_sd_pov_hs & Mean.Score_Physical.Settings.Earth.Science >= one_sd_test_ES)
outlier_Chemistry <- hs_data %>% filter(Percent.Poverty >= one_sd_pov_hs & Mean.Score_Physical.Settings.Chemistry >= one_sd_test_Chemistry)
outlier_hs_Physics <- hs_data %>% filter(Percent.Poverty >= one_sd_pov_hs & Mean.Score_Physical.Settings.Physics >= one_sd_test_Physics)

#create a dataset with all the outlier data with schools
outlier_data1 <- outlier_Chemistry %>% full_join(outlier_hs_Algebra)
outlier_data <- outlier_data1 %>%  full_join(outlier_hs_Algebra2)

#now want to include/exclude some columns
outlier_data <- outlier_data %>% dplyr::select(c(1:6))
outlier_data$Address <- NA
outlier_data$Website <- NA

#add the addresses and website, since only 3 outlier schools I will enter it manually
outlier_data$Website[1] <- "https://www.languageandinnovation.org/"
outlier_data$Website[2] <- "https://www.internationalcommunityhs.org/"
outlier_data$Website[3] <- "https://www.thewae.org/"

outlier_data$Address[1] <- "925 Astor Ave, Bronx, NY 10469"
outlier_data$Address[2] <- "345 Brook Avenue, Bronx, NY 10454"
outlier_data$Address[3] <- "456 White Plains Road, Third Floor, Bronx, NY 10473"

#save it as a csv
#write.csv(outlier_data, file = "~/PIT-DSC-STEM-Inequality/Outliers/hsoutliers.csv")
