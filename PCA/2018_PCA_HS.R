library(dplyr)
library(factoextra)
library(Rcpp)

#loads in dataset
hs <- read.csv("~/PIT-DSC-STEM-Inequality/data/2018_HS_Data.csv")
#add colum SPCT (salary per classroom teacher)
hs <- hs %>% mutate(SPCT = Classroom.Teachers_2017.18 / (Classroom.Teachers.w..0.3.Years.Experience_2017.18 +
                                                       Classroom.Teachers.w..More.than.3.Years.Experience_2017.18) )


hs <- hs %>% select(-c(1:27,29:30,36,40:44,46))
#take away the rows with NA (can't do PCA otherwise)
hs <- hs %>% na.omit()

#compute PCA
hs.pca <- prcomp(hs, center = TRUE, scale = TRUE)

#showcase the result
summary(hs.pca)

#graph/visualize the variables
fviz_pca_var(hs.pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB","#E7B800", "#FC4E07"),
             repel = TRUE )
