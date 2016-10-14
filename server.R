


function(input, output, session) {

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

  aaa <<-isolate(input$servercnt)
    
  output$count <- renderValueBox({
    valueBox(
      value = serverCost(aaa),
      subtitle = "Time to complete",icon = icon("clock-o")
    )
  })
  

}

