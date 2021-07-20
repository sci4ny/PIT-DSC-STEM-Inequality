#server for our app
library(shiny)


shinyServer( function(input, output){
     #want reactive object because our map/results will change depending on user
     #input
     school_data <- reactive({
         d <- hs_data %>% filter(( input$percent_poverty[1] <= Percent.Poverty & Percent.Poverty <= input$percent_poverty[2]) &
                                         (Borough %in% input$borough) &
                                         (input$ccAlgebra[1] <= Mean.Score_Common.Core.Algebra & Mean.Score_Common.Core.Algebra <= input$ccAlgebra[2])
                                        &
                                         (input$ccGeometry[1] <= Mean.Score_Common.Core.Geometry & Mean.Score_Common.Core.Geometry <= input$ccGeometry[2] )
                                        &
                                         ((input$ccLE[1] <= Mean.Score_Living.Environment & Mean.Score_Living.Environment <= input$ccLE[2]) ))
         return(d)
     })

     #The HS tab will output the map based on user's input
     output$HS <- renderLeaflet({
                #make the points of the school a certain shade of the color red depending
                #on Percent Poverty
                pal <- colorBin(palette = c('#fff7ec','#fee8c8','#fdd49e','#fdbb84','#fc8d59','#ef6548','#d7301f','#b30000','#7f0000'),9, domain = hs_data$Percent.Poverty)


                #this creates a leaflet map to be displayed
                leaflet() %>%
                     addTiles() %>%
                     addCircleMarkers(data = school_data() , lng = ~Longitude, lat = ~Latitude, color = ~pal(Percent.Poverty),
                                      popup = ~paste("<h3 style = 'color: #2a52be'>School Information</h3>",
                                                     "<b> DBN: </b>",
                                                     dbn,
                                                     "<br>",
                                                     "<b>School Name: </b>",
                                                     school_name,
                                                     "<br>",
                                                     "<b> Mean Score for Algebra Regent: </b> ",
                                                     Mean.Score_Common.Core.Algebra,
                                                     "<br>",
                                                     "<b> Mean Score for Geometry Regent: </b> ",
                                                     Mean.Score_Common.Core.Geometry,
                                                     "<br>",
                                                     "<b> Mean Score For Living Environment Regent: </b>",
                                                     Mean.Score_Living.Environment,
                                                     "<br>",
                                                     "<b>% of Students in Poverty: </b>",
                                                     paste(Percent.Poverty,"%"),
                                                     "<br>",
                                                     "<b> Total School Funding Per Pupil: </b>",
                                                     Total.School.Funding.per.Pupil_2017.18,
                                                     "<br>",
                                                     "<b> Salary Per Classroom Teache: </b>",
                                                     `Salary Per Classroom Teacher`,
                                                     "<br>",
                                                     "<b> Ratio of STEM teachers to Students: </b>",
                                                     Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students,
                                                     "<br>",
                                                     "<b> STEM AP Courses Offered: </b>",
                                                     STEM_AP_Courses_Offered,
                                                     "<br>",
                                                     "<b> STEM Clubs Offered: </b>",
                                                     club_number,
                                                     sep = " ")) %>%
                #adds a bottom right label that displays the color with the corresponding
                #Percentage of students in poverty
                addLegend(position = "bottomright",
                          pal = pal,
                          values = school_data()$Percent.Poverty,
                          title = "Percentage of Students in Poverty",
                          opacity = 0.7,
                          )


     })



})








