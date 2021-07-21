#server for our app


shinyServer( function(input, output){
        #want reactive object because our map/results will change depending on user
        #input
        school_data <- reactive({
                #creates conditional, if current tab is High School, work with High School data
                if (input$CurrentTab == "Middle School") {
                        d <- ms_df %>% filter(( input$percent_poverty[1] <= `Poverty Percent` & `Poverty Percent` <= input$percent_poverty[2]) & (Borough %in% input$borough) & (input$`TSFPP`[1] <= `TSFPP` & `TSFPP` <= input$`TSFPP`[2]) & (input$`Scaled Mean Score`[1] <= `Scaled Mean Score` & `Scaled Mean Score` <= input$`Scaled Mean Score`[2]))
                        return(d) }
        })

        #The HS tab will output the map based on user's input
        output$MS <- renderLeaflet({
                #make the points of the school a certain shade of the color red depending
                #on Percent Poverty
                pal <- colorBin(palette = c('#fff7ec','#fee8c8','#fdd49e','#fdbb84','#fc8d59','#ef6548','#d7301f','#b30000','#7f0000'),9, domain = ms_df$`Poverty Percent`)


                #this creates a leaflet map to be displayed
                leaflet() %>%
                        addTiles() %>%
                        addCircleMarkers(data = school_data() , lng = ~Longitude, lat = ~Latitude, color = ~pal(`Poverty Percent`),
                                         popup = ~paste("<h3 style = 'color: #2a52be'>School Information</h3>",
                                                        "<b> DBN: </b>",
                                                        DBN,
                                                        "<br>",
                                                        "<b>School Name: </b>",
                                                        `School Name`,
                                                        "<br>",
                                                        "<b>Percent in Poverty: </b>",
                                                        paste(`Poverty Percent`,"%"),
                                                        "</br>",
                                                        "<b> Total School Funding Per Pupil: </b>",
                                                        TSFPP,
                                                        "<br>",
                                                        "<b> Ratio of STEM teachers to Students: </b>",
                                                        `Ratio of Full Time Licensed STEM Teachers to Students*`,
                                                        "<br>",
                                                        "<b> STEM Clubs Offered: </b>",
                                                        `Stem Activities Count`,
                                                        sep = " ",
                                                        "</br>",
                                                        "<b> Scaled Mean Math Test Scores </b>",
                                                        `Scaled Mean Score`,
                                                        "</br>")) %>%
                        #adds a bottom right label that displays the color with the corresponding
                        #Percentage of students in poverty
                        addLegend(position = "bottomright",
                                  pal = pal,
                                  values = school_data()$`Poverty Percent`,
                                  title = "Percentage of Students in Poverty",
                                  opacity = 0.7
                        )


        })



})
