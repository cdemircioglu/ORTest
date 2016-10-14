library(shiny)
library(shinySignals)
library(dplyr)
library(shinydashboard)
library(bubbles)



function(input, output, session) {
  
# An empty prototype of the data frame we want to create
prototype <- data.frame(date = character(), time = character(),size = numeric(), r_version = character(), r_arch = character(),r_os = character(), package = character(), version = character(),country = character(), ip_id = character(), received = numeric())

  # pkgStream is a reactive expression that represents a stream of
  # new package download data; up to once a second it may return a
  # data frame of new downloads since the last update.
  #pkgStream <- packageStream(session)
  #pkgData <- packageData(pkgStream, 100)

  
  #output$rate <- renderValueBox({
  #  valueBox(
  #    value = formatC(nrow(pkgData()), digits = 1, format = "f"),
  #    subtitle = "Percent completed",icon = icon("percent")
  #  )
  #})

  output$count <- renderValueBox({
    valueBox(
      value = input$servercnt,
      subtitle = "Time to complete",icon = icon("clock-o")
    )
  })
  

}

