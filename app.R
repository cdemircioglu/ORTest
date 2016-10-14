library(shiny)
library(shinySignals)
library(dplyr)
library(shinydashboard)
library(bubbles)
#source("C:/Users/c81021292/Desktop/Work/ORDemo/bloomfilter.R")
source("bloomfilter.R")

#if (interactive()) {
  
  ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(
      sliderInput("servercnt", "Number of computers",min = 0, max = 20, value = 3, step = 1)
    ),
    dashboardBody(
      # Boxes need to be put in a row (or column)
      fluidRow(
        valueBoxOutput("completePercent", width=3),
        valueBoxOutput("timetoComplete", width=3)
      )
    )
  )
  
  server <- function(input, output, session) {
    
#######VARIABLES SECTION#######    

    timeRequired <- 80000 #seconds to complete
    
    # Record the time that the session started.
    startTime <- as.numeric(Sys.time())

    # An empty prototype of the data frame we want to create
    prototype <- data.frame(date = character(), time = character(),size = numeric(), r_version = character(), r_arch = character(),r_os = character(), package = character(), version = character(),country = character(), ip_id = character(), received = numeric())

#######OUTPUT SECTION#######    
    
    output$completePercent <- renderValueBox({
      
      valueBox(
        value = formatC(5, digits = 1, format = "f"),
        subtitle = "Percent completed",
        icon = icon("percent"),
        color = "yellow" 
      )
    })
      
    output$timetoComplete <- renderValueBox({
        invalidateLater(1000, session)
        elapsed <- as.numeric(Sys.time()) - startTime
        timeRequired <- timeRequired - elapsed*input$servercnt
        timeleft <- timeRequired/(input$servercnt)
        
        valueBox(
          value = format(as.POSIXct('2016-01-01 00:00:00') + timeleft, "%H:%M:%S"),
          subtitle = "Time to complete",
          icon = icon("time")
        )
    })
    
    
    
    
  }

  
  
#  shinyApp(ui, server)
#}

