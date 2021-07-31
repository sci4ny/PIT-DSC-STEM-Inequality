#server for our app
library(shiny)


shinyServer( function(input, output){
        #want reactive object because our map/results will change depending on user input

        school_data <- reactive({

                #creates condition, if current tab is All, work with master dataset
                if (input$CurrentTab == "All (Middle&Elementary)") {
                        d <- master_data %>% filter(( input$percent_poverty[1] <= Percent.Poverty & Percent.Poverty <= input$percent_poverty[2])
                                                    & (Borough %in% input$borough)
                                                    & (input$FundingPerPupil[1] <= Total.School.Funding.per.Pupil & Total.School.Funding.per.Pupil <= input$FundingPerPupil[2])
                                                    & (input$Scale.Score.Slider[1] <= Mean.Scale.Math.Score & Mean.Scale.Math.Score <= input$Scale.Score.Slider[2]))
                        return(d)}





                #creates conditional, if current tab is High School, work with High School data
                else if (input$CurrentTab == "High School") {
                        d <- hs_data %>% filter(( input$percent_poverty[1] <= Percent.Poverty & Percent.Poverty <= input$percent_poverty[2]) &
                                                        (Borough %in% input$borough) &
                                                        (input$ccAlgebra[1] <= Mean.Score_Common.Core.Algebra & Mean.Score_Common.Core.Algebra <= input$ccAlgebra[2])
                                                &
                                                        (input$ccGeometry[1] <= Mean.Score_Common.Core.Geometry & Mean.Score_Common.Core.Geometry <= input$ccGeometry[2] )
                                                &
                                                        ((input$ccLE[1] <= Mean.Score_Living.Environment & Mean.Score_Living.Environment <= input$ccLE[2]) ))
                        return(d)}

                #if current tab is Middle School, work with Middle School data
                else if (input$CurrentTab == "Middle School") {
                        d <- ms_df %>% filter(( input$percent_poverty[1] <=  `Poverty Percent` & `Poverty Percent` <= input$percent_poverty[2])
                                              & (Borough %in% input$borough)
                                              & (input$`TSFPP`[1] <= TSFPP & TSFPP <= input$`TSFPP`[2])
                                              & (input$`Scaled Mean Score`[1] <= `Scaled Mean Score` & `Scaled Mean Score` <= input$`Scaled Mean Score`[2]))
                        return (d) }


                #if current tab is Elementary School, work with Elementary School data
                else if (input$CurrentTab == "Elementary School") {
                        d <- elem_data %>% filter((input$percent_poverty[1] <= Pov & Pov <= input$percent_poverty[2])
                                                  & (Borough %in% input$borough)
                                                  & (input$FPP[1] <= FPP & FPP <= input$FPP[2])
                                                  & (input$MSS.All.Grades.Slider[1] <= MSS.All.Grades & MSS.All.Grades <= input$MSS.All.Grades.Slider[2]))
                        return (d) }

                else if (input$CurrentTab == "Outliers") {
                        d <- outlier_data
                        return(d)
                }
        })


        #Map for the All Tab
        output$All <- renderLeaflet({
                #make the points of the school a certain color depending on their school type
                pal <- colorFactor(palette = c('#336601','#D64A97','#ff6003','#3242a3','#653300'), domain = master_data$School.Type)


                #create the leaflet map
                leaflet() %>%
                        addTiles() %>%
                        addCircleMarkers(data = school_data(), lng = ~Longitude, lat = ~Latitude, color = ~pal(School.Type),
                                         popup = ~paste("<h3 style = 'color: #2a52be'>School Information</h3>",
                                                        "<b>DBN:</b>",
                                                         dbn,
                                                        "<br>",
                                                        "<b>School Name:</b>",
                                                        School.Name,
                                                        "<br>",
                                                        "<b> Grade Span: </b>",
                                                        gradespan,
                                                        "<br>",
                                                        "<b>% of Students in Poverty: </b>",
                                                        paste(round(Percent.Poverty,2), "%"),
                                                        "<br>",
                                                        "<b>Mean Scale Math Score: </b>",
                                                        round(Mean.Scale.Math.Score,2),
                                                        "<br>",
                                                        "<b> Ratio of STEM Teachers to Students: </b>",
                                                        Ratio.of.Full.Time.Licensed.STEM.Teachers.to.Students,
                                                        sep = " ")) %>%
                        addLegend(position = "bottomright",
                                  pal = pal,
                                  values = school_data()$School.Type,
                                  title = "School Type",
                                  opacity = .7,
                        )





        })




        #Map for the High School tab
        output$HS <- renderLeaflet({
                #make the points of the school a certain shade of the color red depending
                #on Percent Poverty
                pal <- colorBin(palette = c('#ffab26','#fe7701','#ff6003','#f52e00','#b82020'),9, domain = hs_data$Percent.Poverty)


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
                                  opacity = .7,
                        )

        })



        output$MS <- renderLeaflet({
                #make the points of the school a certain shade of the color blue depending
                #on Percent Poverty
                pal <- colorBin(palette = c('#5cafc9','#437dba','#3242a3','#1b207a'),9, domain = ms_df$`Poverty Percent`)

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
                                                        "<b>Ratio of STEM Teachers to Students: </b>",
                                                        `Ratio of Full Time Licensed STEM Teachers to Students*`,
                                                        "</br>",
                                                        "<b> STEM Clubs Offered: </b>",
                                                        `Stem Activities Count`,
                                                        "</br>",
                                                        "<b> Scaled Mean Math Test Scores </b>",
                                                        round(`Scaled Mean Score`,2),
                                                        "</br>", sep = " ")) %>%
                        #adds a bottom right label that displays the color with the corresponding
                        #Percentage of students in poverty
                        addLegend(position = "bottomright",
                                  pal = pal,
                                  values = school_data()$`Poverty Percent`,
                                  title = "Percentage of Students in Poverty",
                                  opacity = 0.7
                        )


        })


        #Map for the Elementary School tab
        output$ES <- renderLeaflet({
                #make the points of the school a certain shade of a pinkish-red color depending
                #on Percent Poverty
                pal <- colorBin(palette = c('#F58FBD', '#EB78B0', '#E061A4', '#D64A97','#CC338B' ),9, domain = elem_data$Pov)


                #this creates a leaflet map to be displayed
                leaflet() %>%
                        addTiles() %>%
                        addCircleMarkers(data = school_data(), lng = ~Longitude, lat = ~Latitude, color = ~pal(Pov),
                                         popup = ~paste("<h3 style = 'color: #2a52be'>School Information</h3>",
                                                        "<b>DBN:</b>", DBN,
                                                        "<br>",
                                                        "<b>School Name:</b>", Name.y,
                                                        "<br>",
                                                        "<b>% of Students in Poverty: </b>",
                                                        paste(round(Pov,2), "%"),
                                                        "<br>",
                                                        "<b>Mean Scale Score, Grades 3-5: </b>",
                                                        round(MSS.All.Grades,2),
                                                        "<br>",
                                                        "<b> Ratio of STEM Teachers to Students: </b>",
                                                        RTS,
                                                        sep = " "))%>%
                        #adds a bottom right label that displays the color with the corresponding
                        #Percentage of students in poverty
                        addLegend(position = "bottomright",
                                  pal = pal,
                                  values = school_data()$Pov,
                                  title = "Percentage of Students in Poverty",
                                  opacity = 0.7,
                        )

        })

        output$OUT <- renderLeaflet({

                leaflet() %>%
                        addTiles() %>%
                        addCircleMarkers(data = school_data(), lng = ~Longitude, lat = ~Latitude, color = "#1FA808", opacity = .7,
                                                         popup = ~paste("<h3 style = 'color: #2a52be'>School Information</h3>",
                                                                        "<b>DBN:</b>",
                                                                        dbn,
                                                                        "<br>",
                                                                        "<b>School Name: </b>",
                                                                        school_name,
                                                                        "<br>",
                                                                        "<b> Address:  </b>",
                                                                        Address,
                                                                        "<br>",
                                                                        "<b>Grade Span: </b>",
                                                                        grade_span,
                                                                        "<br>",
                                                                        "<b> Website: </b>",
                                                                        paste0("<a href='",Website,"'>",Website,"</a>"),
                                                                        sep = " "))


        })


        #bottom of tab displays current number of schools on the map
        output$totalSchools <- renderText({
                paste("Number of Schools Currently Displayed: ", nrow(school_data()) )
        })

})

