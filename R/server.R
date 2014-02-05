library(shiny)

fileName <- '/home/natalie/observations_paper/bsb_obs_alldata.txt'
d<-readData(fileName)


# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  # Compute the forumla text in a reactive expression since it is 
  # shared by the output$caption and output$mpgPlot expressions
  formulaText <- reactive({
    paste(input$variable, ": Wind Speed")
  })

  # Return the formula text for printing as a caption
  output$caption <- renderText({
    formulaText()
  })

  # Generate a plot of the requested sensor speed 
  output$speedPlot <- renderPlot({
    s<-subset(d, subset=(plot==input$variable))
    p<-plotSensorSpeed(d, input$variable)
    print(p)
  })
  
  output$vectorMap <- renderPlot({
    dsub<-subsetOnSpeed(d, 'R2', '>', 6.0)
    davg<- buildHourlyAverages(dsub)
    h <- c(0, 1, 2, 3, 4, 5)
    dsubhour <- subsetOnHour(davg, h)
    #bsb
    lat = 43.402726
    lon = -113.027724
    zoom = 13
    maptype = 'terrain'

    m<-makeVectorMap(dsubhour, lat, lon, zoom, maptype, colorscale='continuous', axis_labels=FALSE)
    print(m)
   })
  
  

})


