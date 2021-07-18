#server for our app
library(shiny)

shinyServer( function(input, output){
     #we want to output a map based on the grade_range input
     output$map <- renderLeaflet({
          #this is a template, shows that High school map works when selecting High school input
           if (input$grade_range == "High School"){
               hs_map
           }
     })



})








