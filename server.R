
function(input, output, session) {

  # pkgStream is a reactive expression that represents a stream of
  # new package download data; up to once a second it may return a
  # data frame of new downloads since the last update.
  pkgStream <- packageStream(session)
  
  # pkgData is a reactive expression that accumulates previous
  # values of pkgStream, discarding any that are older than
  # maxAgeSecs.
  pkgData <- packageData(pkgStream, 100)
  
  output$rate <- renderValueBox({
    # The downloadRate is the number of rows in pkgData since
    # either startTime or maxAgeSecs ago, whichever is later.
    valueBox(
      value = formatC(nrow(pkgData()), digits = 1, format = "f"),
      subtitle = "Percent completed",icon = icon("percent")
    )
  })
  
  output$count <- renderValueBox({
    valueBox(
      value = max(nrow(pkgData()), input$rateThreshold*100),
      subtitle = "Time to complete",icon = icon("clock-o")
    )
  })
  



}

