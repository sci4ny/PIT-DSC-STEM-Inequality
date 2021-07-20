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
               #group box that allows user to select boroughs
               checkboxGroupInput("borough", label = "Borough to Include: ",
                                  choices = c("BRONX","BROOKLYN","MANHATTAN","QUEENS","STATEN IS"),
                                  selected = c("BRONX","BROOKLYN","MANHATTAN","QUEENS","STATEN IS")),
               #creates a slider that select what range of % poverty the user wants to see
               sliderInput("percent_poverty",
                           label = "Percent Poverty Range: ",
                           min = 0, max = 100, value = c(0,100)),
               #creates slider input for STEM regent scores
               sliderInput("ccAlgebra",
                           label = "Mean Score for Common Core Algebra Range: ",
                           min = 0, max = 100, value = c(0,100)),
               sliderInput("ccGeometry",
                           label = "Mean Score for Common Core Geometry Range: ",
                           min = 0, max = 100, value = c(0,100)),

               sliderInput("ccLE",
                           label = "Mean Score for Living Environment Range: ",
                           min = 0, max = 100, value = c(0,100)),
          ),
          #main panel will showcase the output (map)
          mainPanel(
                #we create different panels for different grade ranges
                tabsetPanel(id = "CurrentTab",
                  tabPanel(title = "High School" , leafletOutput(outputId = "HS"))
          )
     )
   )
 )
)

