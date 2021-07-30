#ui for our app
library(shiny)
library(dplyr)
library(leaflet)
library(here)
#run the code tha will load our data
source(here("ShinyApp", "dataLoad.R"))


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


                                #conditional panel for All Tab
                                conditionalPanel( condition = "input.CurrentTab == 'All (Middle&Elementary)'",

                                                  #creates slider
                                                  sliderInput("FundingPerPupil",
                                                              label = "Total School Funding Per Pupil: ",
                                                              min = 15000, max = 50000, value = c(15000,50000)),
                                                  sliderInput("Scale.Score.Slider",
                                                              label = "Mean Scale Score for State Math Tests: ",
                                                              min = 550, max = 650, value = c(550,650))

                                ),


                                #because we want to display different sliders for specific grade ranges,
                                #we only include these sliders when the tab selected is high school
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

                                #conditional panel for Middle School
                                conditionalPanel( condition = "input.CurrentTab == 'Middle School'",

                                                  #creates slider
                                                  sliderInput("TSFPP",
                                                              label = "Total School Funding Per Pupil: ",
                                                              min = 15000, max = 50000, value = c(15000,50000)),
                                                  sliderInput("Scaled Mean Score",
                                                              label = "Mean Scale Score for State Math Tests: ",
                                                              min = 0, max = 700, value = c(0,700))
                                ),

                                #conditional panel for Elementary School
                                conditionalPanel( condition = "input.CurrentTab == 'Elementary School'",

                                                  #creates slider
                                                  sliderInput("FPP",
                                                              label = "Total School Funding Per Pupil: ",
                                                              min = 15000, max = 50000, value = c(15000,50000)),
                                                  sliderInput("MSS.All.Grades.Slider",
                                                              label = "Mean Scale Score for State Math Tests: ",
                                                              min = 550, max = 650, value = c(550,650))

                                )),

                        #main panel will showcase the output (map)
                        mainPanel(

                                #we create different panels for different grade ranges
                                tabsetPanel(id = "CurrentTab",
                                            tabPanel(title = "All (Middle&Elementary)" , leafletOutput(outputId = "All")),
                                            tabPanel(title = "High School" , leafletOutput(outputId = "HS")),
                                            tabPanel(title = "Middle School" , leafletOutput(outputId = "MS")),
                                            tabPanel(title = "Elementary School" , leafletOutput(outputId = "ES")),
                                            tabPanel(title = "Outliers", leafletOutput(outputId = "OUT")),
                                            #this prevents error message form showing up when switching tabs
                                            tags$style(type="text/css",
                                                       ".shiny-output-error { visibility: hidden; }",
                                                       ".shiny-output-error:before { visibility: hidden; }")
                                ),

                                #this is to output the number of schools currently displayed on the map
                                verbatimTextOutput(outputId = "totalSchools")

                        )
                )
        )
)

