#ui for our app
library(shiny)
library(dplyr)
library(leaflet)
#run the code tha will load our data
source("dataLoad.r")


shinyUI(


fluidPage(
     #Title of Application
     titlePanel("NYC Schools"),
     #we have a sidebar with the grade range input and slider input
     sidebarLayout(
          sidebarPanel(
               h5("Data shown for each school are the most relevant metrics (found via MLR)
                  in predicting the outcome of STEM regents or math test results.")
                ,
                #this creates an option for the user to select what grade range they want to see
                selectInput("grade_range",
                            "Select a Grade Range:",
                            choices = c("All","High School", "Middle School", "Elementary School")
                            ),
               #creates a slider that select what range of % poverty the user wants to see
               sliderInput("percent_poverty",
                           label = "Percent Poverty Range: ",
                           min = .0, max = .95, value = c(.0,.95))
          ),
          #main panel will showcase the output (map)
          mainPanel(
                  leafletOutput(outputId = "map")
          )
     )
)
)
