library(shiny)
library(tidyverse)
library(leaflet)


shinyUI(

fluidPage(
     #Title of Application
     titlePanel("NYC Schools"),
     #we have a sidebar with the grade range input
     sidebarLayout(
          sidebarPanel(
               h5("Data shown for each school are the most relevant metrics (found via MLR)
                  in predicting the outcome of STEM regents or math test results.")
                ,
                selectInput("grade_range",
                            "Select a grade range:",
                            choices = c("All","High School", "Middle School", "Elementary School"))
          ),
          #main panel will showcase the output (map)
          mainPanel(
                  leafletOutput(outputId = "map")
          )
     )
)
)
