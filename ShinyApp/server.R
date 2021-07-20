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

     #we want to output a map based on the grade_range input
     output$map <- renderLeaflet({
          #this is a template, shows that High school map works when selecting High school input
           if (input$grade_range == "High School"){
                leaflet() %>%
                     addTiles() %>%
                     addCircleMarkers(data = school_data() , lng = ~Longitude, lat = ~Latitude, color = "#2a52be",
                                      popup = ~paste("<h3 style = 'color: #2a52be'>School Information</h3>",
                                                     "<b>School Name: </b>",
                                                     school_name,
                                                     "<br>",
                                                     "<b>% of Students in Poverty: </b>",
                                                     paste(Percent.Poverty * 100,"%"),
                                                     "<br>",
                                                     "<b> Ratio of STEM teachers to Students: </b>",
                                                     Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students,
                                                     "<br>",
                                                     "<b> STEM AP Courses Offered: </b>",
                                                     STEM_AP_Courses_Offered,
                                                     sep = " "))

           }
     })



})








