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

               #because we want to display different sliders for specific grade ranges,
               #we only include these sliders, when the tab selected is high school
               conditionalPanel( condition = "input.CurrentTab == 'High School'",

               #creates slider input for STEM regent scores
               sliderInput("ccAlgebra",
                           label = "Mean Score for Common Core Algebra Range: ",
                           min = 0, max = 100, value = c(0,100)),
               sliderInput("ccGeometry",
                           label = "Mean Score for Common Core Geometry Range: ",
                           min = 0, max = 100, value = c(0,100)),

               sliderInput("ccLE",
                           label = "Mean Score for Living Environment Range: ",
                           min = 0, max = 100, value = c(0,100))
          ),
          conditionalPanel( condition = "input.CurrentTab == 'Middle School'",

                            #creates slider
                            sliderInput("TSFPP",
                                        label = "Total School Funding Per Pupil",
                                        min = 15000, max = 50000, value = c(15000,50000)),
                            sliderInput("Scaled Mean Score",
                                        label = "Scaled Mean Math Score: ",
                                        min = 0, max = 500, value = c(0,500))
          )),

          #main panel will showcase the output (map)
          mainPanel(
                #we create different panels for different grade ranges
                tabsetPanel(id = "CurrentTab",
                  tabPanel(title = "High School" , leafletOutput(outputId = "HS")),
                  tabPanel(title = "Middle School" , leafletOutput(outputId = "MS")),
                  #this prevents error message form showing up when switching tabs
                  tags$style(type="text/css",
                             ".shiny-output-error { visibility: hidden; }",
                             ".shiny-output-error:before { visibility: hidden; }")
          )
     )
   )
 )
)

