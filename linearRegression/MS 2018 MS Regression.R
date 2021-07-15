library(readxl)
library(Hmisc)
library(rstatix)
library(tidyr)
library(dplyr)
library(psych)
library(broom)
library(sjPlot)
library(writexl)
source("http://www.sthda.com/upload/rquery_cormat.r")

#-------- LOADING / SELECTION -----------
#load directory, convert to matrix (for rcorr)
directory <- read_excel("/Users/nathaniellowe/Documents/STEM Research/Import/Exports/2018 MS Metrics (Complete).xlsx")
corr_directory <- directory[-c(1:6)] 
corr_directory <- corr_directory %>% mutate(PD17a = as.integer(PD17a), PD17b = as.integer(PD17b), PD17c = as.integer(PD17c))

#change names to fit better

#------- CORRELATION AND PLOT -------------
matrix <- rcorr(as.matrix(corr_directory), type = 'pearson')
number <- cor_plot(matrix$r, method = 'number')
#color <- cor_plot(matrix$r, method = 'color')

#-------- CONFIDENCE & P -----------
`Math Proficiency` <- lm(`Math Proficiency` ~ `# of Comp Sci Courses`  + `Teacher Student Ratio` + `Mbps Bandwidth` + `PD17a` + `PD17b` + `PD17c` + `Scaled Mean Score` + `Poverty Percent` + `FRPL` + `TSFPP` + `Average Teacher Salary` + `PLVL34` + `Stem Activities Count`, corr_directory)
`# of Comp Sci Courses` <- lm(`# of Comp Sci Courses` ~ `Math Proficiency`  + `Teacher Student Ratio` + `Mbps Bandwidth` + `PD17a` + `PD17b` + `PD17c` + `Scaled Mean Score` + `Poverty Percent` + `FRPL` + `TSFPP` + `Average Teacher Salary` + `PLVL34` + `Stem Activities Count`, corr_directory)
`Teacher Student Ratio` <- lm(`Teacher Student Ratio` ~ `# of Comp Sci Courses`  + `Math Proficiency` + `Mbps Bandwidth` + `PD17a` + `PD17b` + `PD17c` + `Scaled Mean Score` + `Poverty Percent` + `FRPL` + `TSFPP` + `Average Teacher Salary` + `PLVL34` + `Stem Activities Count` + `PLVL34` + `Stem Activities Count`, corr_directory)
`Mbps Bandwidth` <- lm(`Mbps Bandwidth` ~ `# of Comp Sci Courses`  + `Teacher Student Ratio` + `Math Proficiency` + `PD17a` + `PD17b` + `PD17c` + `Scaled Mean Score` + `Poverty Percent` + `FRPL` + `TSFPP` + `Average Teacher Salary` + `PLVL34` + `Stem Activities Count`, corr_directory)
`PD17a` <- lm(`PD17a` ~ `# of Comp Sci Courses`  + `Teacher Student Ratio` + `Mbps Bandwidth` + `Math Proficiency` + `PD17b` + `PD17c` + `Scaled Mean Score` + `Poverty Percent` + `FRPL` + `TSFPP` + `Average Teacher Salary` + `PLVL34` + `Stem Activities Count`, corr_directory)
`PD17b` <- lm(`PD17b` ~ `# of Comp Sci Courses`  + `Teacher Student Ratio` + `Mbps Bandwidth` + `PD17a` + `Math Proficiency` + `PD17c` + `Scaled Mean Score` + `Poverty Percent` + `FRPL` + `TSFPP` + `Average Teacher Salary` + `PLVL34` + `Stem Activities Count`, corr_directory)
`PD17c` <- lm(`PD17c` ~ `# of Comp Sci Courses`  + `Teacher Student Ratio` + `Mbps Bandwidth` + `PD17a` + `PD17b` + `Math Proficiency` + `Scaled Mean Score` + `Poverty Percent` + `FRPL` + `TSFPP` + `Average Teacher Salary` + `PLVL34` + `Stem Activities Count`, corr_directory)
`Scaled Mean Score` <- lm(`Scaled Mean Score` ~ `# of Comp Sci Courses`  + `Teacher Student Ratio` + `Mbps Bandwidth` + `PD17a` + `PD17b` + `PD17c` + `Math Proficiency` + `Poverty Percent` + `FRPL` + `TSFPP` + `Average Teacher Salary` + `PLVL34` + `Stem Activities Count`, corr_directory)
`Poverty Percent` <- lm(`Poverty Percent` ~ `# of Comp Sci Courses`  + `Teacher Student Ratio` + `Mbps Bandwidth` + `PD17a` + `PD17b` + `PD17c` + `Math Proficiency` + `Scaled Mean Score` + `FRPL` + `TSFPP` + `Average Teacher Salary` + `PLVL34` + `Stem Activities Count`, corr_directory)
`FRPL` <- lm(`FRPL` ~ `# of Comp Sci Courses`  + `Teacher Student Ratio` + `Mbps Bandwidth` + `PD17a` + `PD17b` + `PD17c` + `Scaled Mean Score` + `Poverty Percent` + `Math Proficiency` + `TSFPP` + `Average Teacher Salary` + `PLVL34` + `Stem Activities Count`, corr_directory)
`TSFPP` <- lm(`TSFPP` ~ `# of Comp Sci Courses`  + `Teacher Student Ratio` + `Mbps Bandwidth` + `PD17a` + `PD17b` + `PD17c` + `Scaled Mean Score` + `Poverty Percent` + `FRPL` + `Math Proficiency` + `Average Teacher Salary` + `PLVL34` + `Stem Activities Count`, corr_directory)
`Average Teacher Salary` <- lm(`Average Teacher Salary` ~ `# of Comp Sci Courses`  + `Teacher Student Ratio` + `Mbps Bandwidth` + `PD17a` + `PD17b` + `PD17c` + `Scaled Mean Score` + `Poverty Percent` + `FRPL` + `TSFPP` + `Math Proficiency` + `PLVL34` + `Stem Activities Count`, corr_directory)
`PLVL34` <- lm(`PLVL34` ~ `# of Comp Sci Courses`  + `Teacher Student Ratio` + `Mbps Bandwidth` + `PD17a` + `PD17b` + `PD17c` + `Scaled Mean Score` + `Poverty Percent` + `FRPL` + `TSFPP` + `Math Proficiency` + `Average Teacher Salary` + `Stem Activities Count`, corr_directory)
`Stem Activities Count` <- lm(`Stem Activities Count` ~ `# of Comp Sci Courses`  + `Teacher Student Ratio` + `Mbps Bandwidth` + `PD17a` + `PD17b` + `PD17c` + `Scaled Mean Score` + `Poverty Percent` + `FRPL` + `TSFPP` + `Math Proficiency` + `PLVL34` + `Average Teacher Salary`, corr_directory)

tab_model(`Math Proficiency`,  `# of Comp Sci Courses`  , `Teacher Student Ratio` , `Mbps Bandwidth` , `PD17a` , `PD17b` , `PD17c` , `Scaled Mean Score` , `Poverty Percent` , `FRPL` , `TSFPP` , `Average Teacher Salary`, `PLVL34`, `Stem Activities Count`)