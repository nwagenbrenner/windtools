library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("High-resolution Wind Data"),

  sidebarPanel(
    selectInput("variable", "Variable:",
                list("R2" = "R2", 
                     "TSW8" = "TSW8", 
                     "R5" = "R5")),

    checkboxInput("outliers", "Show outliers", FALSE)
  ),

  # Show the caption and plot of the requested variable against speed
  mainPanel(
    h3(textOutput("caption")),

    plotOutput("speedPlot"),
    
    plotOutput("vectorMap")
  )
))


