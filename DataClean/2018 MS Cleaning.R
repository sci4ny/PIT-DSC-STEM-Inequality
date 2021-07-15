#------ PACKAGE LOADING ------
library(dplyr)
library(readxl)
library(writexl)
library(stringr)
library(naniar)
library(magrittr)

#----- DIRECTORY LOADING -----
load <- read_excel("/Users/nathaniellowe/Documents/STEM Research/Import/Datasets/Directory.xlsx")
directory <-load %>% select('schooldbn', 'printedschoolname', 'Borough', 'Latitude', 'Longitude', 'activities', 'mathprof')

#----- LOAD CSR ----- 
csr1 <- read_excel("/Users/nathaniellowe/Documents/STEM Research/Import/Datasets/Computer Science Report.xlsx", sheet = "CS Programs", col_types = c("text", "skip", "numeric", "skip", "skip", "skip"))
csr2 <- read_excel("/Users/nathaniellowe/Documents/STEM Research/Import/Datasets/Computer Science Report.xlsx", sheet = "STEM Teachers", col_types = c("text", "skip", "text", "skip", "skip", "skip", "text"), skip = 3)
#convert teacher:student ratio to decimal
numerator <- as.integer(sub(":.*", "", csr2$`Ratio of Full Time Licensed STEM Teachers to Students*`))
denominator <- as.integer(sub(".*:", "", csr2$`Ratio of Full Time Licensed STEM Teachers to Students*`))
csr2$`Ratio of Full Time Licensed STEM Teachers to Students*` <- numerator / denominator
csr3 <- read_excel("/Users/nathaniellowe/Documents/STEM Research/Import/Datasets/Computer Science Report.xlsx", sheet = "School Bandwidth", col_types = c("skip","text", "skip", "numeric"), skip = 3)

# #----- LOAD FUNDING ----- 
funding <- read_excel("/Users/nathaniellowe/Documents/STEM Research/Import/Datasets/Funding.xlsx", sheet = "Sheet1") %>% select(5,17,20:21,28,49)
is.na(funding) <- funding == "n/a"
funding$"Average Teacher Salary" <- as.numeric(funding$`Classroom Teachers`) / (as.numeric(funding$`Classroom Teachers w/ 0-3 Years Experience`) + as.integer(funding$`Classroom Teachers w/ More than 3 Years Experience`))
funding <- funding %>% select(-c(3:5)) #delete independent vars of avg teacher salary

#----- LOAD SURVEY -----
survey <- read_excel("/Users/nathaniellowe/Documents/STEM Research/Import/Datasets/Teacher Survey.xlsx") %>% select(1, 173, 175, 177) %>% replace_with_na_all(condition = ~.x == 'N/A') 
survey$`Strongly disagree/Disagree...2` <- as.numeric(survey$`Strongly disagree/Disagree...2`)
survey$`Strongly disagree/Disagree...3` <- as.numeric(survey$`Strongly disagree/Disagree...3`)
survey$`Strongly disagree/Disagree...4` <- as.numeric(survey$`Strongly disagree/Disagree...4`)

#----- LOAD MATH RESULTS -----
math_results <- read_excel("/Users/nathaniellowe/Documents/STEM Research/Import/Datasets/Math Test Results.xlsx") %>% filter(Year == '2018' & Grade %in% c(6,7,8)) %>% select(c(2,7,8,17)) %>% replace_with_na(replace = list(`Mean Scale Score` = 's', `# Level 3+4` = 's')) %>% group_by(DBN) %>% dplyr::summarize(`Scaled Mean Score` = sum(as.numeric(`Number Tested`), na.rm =TRUE) / 3, `Number Tested` = sum(as.numeric(`Number Tested`), na.rm = TRUE), `NLVL34` = sum(as.numeric(`# Level 3+4`), na.rm = TRUE))

#----- LOAD POVERTY -----
poverty<- read.csv("/Users/nathaniellowe/Documents/STEM Research/Import/Datasets/poverty.csv") %>% select(c(1,3,38)) %>% filter(Year == '2017-18') %>% select(c(1,3)) %>% set_colnames(c('DBN', 'Poverty Percent'))
poverty$`Poverty Percent` %<>% gsub("Above ", "", .) %>% gsub("Below ", "", .) %>% gsub("%", "", .)
poverty$`Poverty Percent` <- as.numeric(poverty$`Poverty Percent`)

#----------- JOINING -------------
directory <- directory %>% rename(DBN = schooldbn) %>% left_join(csr1, by = 'DBN') %>% left_join(csr2, by = 'DBN') %>% left_join(csr3, by = 'DBN') %>% left_join(survey, by = 'DBN') %>% left_join(math_results, by = 'DBN') %>% left_join(poverty, by = 'DBN')
#funding added separate bc uses local codes instead of full DBN
trim <- sub('..', '', directory$DBN) #get local codes
directory$dbnlocal <- trim
directory <- left_join(directory, funding, by = c('dbnlocal' = 'Local School Code'))
directory$`PLVL34` <- directory$NLVL34 / directory$`Number Tested`
directory <- directory %>% rename(`School Name` = `printedschoolname`, `Math Proficiency` = `mathprof`, `PD17a` = names(directory)[12], `PD17b` = names(directory)[13], `PD17c` = names(directory)[14], `Teacher Student Ratio` = names(directory)[10], `FRPL` = `K-12 FRPL Count`, `TSFPP` = names(directory)[21]) %>% relocate(`School Type`, .after = `School Name`) %>% select(-c(16,17, 19))

#----- ACTIVITY COUNT -----
activity_count <- function(x){
  low_x <- tolower(x)
  count <- 0 
  for(element in 1:length(keywords)){
    count <- count + str_count(low_x, pattern = keywords[element])}
  return(count)}
keywords <- list("Acoustics", "Aeronautics","Astronomy"," Biology","Botany","Chemistry","Crystallography","Ecology","Entomology","Geography","Geology","Hydrology","Ichthyology","Logic","Mathematics","Mechanics","Meteorology","Metrology","Mineralogy", "Oceanography","Optics","Paleontology","Petrology", "Robotics","Science","Seismology","Volcanology","Zoology", "MATHletes", "DNA", "Coding") %>% tolower()
directory$`Stem Activities Count` <- activity_count(directory$activities)
directory <- directory %>% select(-7)

#----------- NA CLEANUP--------------
na_look_columns <- directory %>% select (7:ncol(directory))
complete_cases <- rowSums(is.na(na_look_columns)) != ncol(na_look_columns)
directory_complete <- directory[complete_cases,]

#----------- SAVE & EXPORT -----------------
write_xlsx(directory_complete,"/Users/nathaniellowe/Documents/STEM Research/Import/Exports/2018 MS Metrics (Complete).xlsx")