
function(input, output, session) {


  # pkgStream is a reactive expression that represents a stream of
  # new package download data; up to once a second it may return a
  # data frame of new downloads since the last update.
  pkgStream <- packageStream(session)
  
  # Max age of data (5 minutes)
  maxAgeSecs <- 60 * 5

  # pkgData is a reactive expression that accumulates previous
  # values of pkgStream, discarding any that are older than
  # maxAgeSecs.
  pkgData <- packageData(pkgStream, maxAgeSecs)
  
  # dlCount is a reactive expression that keeps track of the total
  # number of rows that have ever appeared through pkgStream.
  dlCount <- downloadCount(pkgStream)  

  # usrCount is a reactive expression that keeps an approximate
  # count of all of the unique users that have been seen since the
  # app started.
  usrCount <- userCount(pkgStream)
  
  # Record the time that the session started.
  startTime <- as.numeric(Sys.time())
 
  output$rate <- renderValueBox({
    # The downloadRate is the number of rows in pkgData since
    # either startTime or maxAgeSecs ago, whichever is later.
    elapsed <- as.numeric(Sys.time()) - startTime
    downloadRate <- nrow(pkgData()) / min(maxAgeSecs, elapsed)
    
    valueBox(
      value = formatC(downloadRate, digits = 1, format = "f"),
      subtitle = "Percent completed",
      icon = icon("percent"),
      #color = if (downloadRate >= input$rateThreshold) "yellow" else "aqua"
      color = "yellow" 
    )
  })
  
  output$count <- renderValueBox({
    currentServerCnt   <- max(srvCost(), input$rateThreshold*1.50)
    
    valueBox(
      value = currentServerCnt,
      subtitle = "Time to complete",
      icon = icon("clock-o")
    )
  })
  
  output$packagePlot <- renderBubbles({
    if (nrow(pkgData()) == 0)
      return()
    
    order <- unique(pkgData()$package)
    df <- pkgData() %>%
      group_by(package) %>%
      tally() %>%
      arrange(desc(n), tolower(package)) %>%
      # Just show the top 60, otherwise it gets hard to see
      head(60)
      
    bubbles(df$n, df$package, key = df$package )
  })
  


}

