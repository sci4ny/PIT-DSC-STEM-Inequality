library(readxl)
library(leaflet)

df <- read_excel("/Users/nathaniellowe/Documents/STEM Research/Import/Exports/2018 MS Metrics (Complete).xlsx")
map <- leaflet() %>% addTiles() %>% addCircles(lng = ~Longitude,
                                               lat = ~Latitude,
                                               label = ~`School Name`,
                                               data = df)