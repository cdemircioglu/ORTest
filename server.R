
library(colorspace)

function(input, output, session) {

cx <- function (n, h = c(-243, 360), c = 91, l = c(61, 77), power = 0.833333333333333, 
    fixup = TRUE, gamma = NULL, alpha = 1, ...) 
{
    if (!is.null(gamma)) 
        warning("'gamma' is deprecated and has no effect")
    if (n < 1L) 
        return(character(0L))
    h <- rep(h, length.out = 2L)
    c <- c[1L]
    l <- rep(l, length.out = 2L)
    power <- rep(power, length.out = 2L)
    rval <- seq(1, -1, length = n)
    rval <- hex(polarLUV(L = l[2L] - diff(l) * abs(rval)^power[2L], 
        C = c * abs(rval)^power[1L], H = ifelse(rval > 0, h[1L], 
            h[2L])), fixup = fixup, ...)
    if (!missing(alpha)) {
        alpha <- pmax(pmin(alpha, 1), 0)
        alpha <- format(as.hexmode(round(alpha * 255 + 1e-04)), 
            width = 2L, upper.case = TRUE)
        rval <- paste(rval, alpha, sep = "")
    }
    return(rval)
}


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

  srvCost <- serverCost(pkgStream,1)

  # usrCount is a reactive expression that keeps an approximate
  # count of all of the unique users that have been seen since the
  # app started.
  usrCount <- userCount(pkgStream)
  
  # Record the time that the session started.
  startTime <- as.numeric(Sys.time())

  tmRemain <- timeRemain(pkgStream,startTime,input$rateThreshold)
  
     
  
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

  output$cost <- renderValueBox({

   costRate <- max(srvCost(), input$rateThreshold*1.50)

    valueBox(
      value =  formatC(costRate, digits = 2, format = "f"),
      subtitle = "Cost per hour",
      icon = icon("usd")
    )
  })

  
  output$users <- renderValueBox({
    valueBox(
      usrCount(),
      "Unique customers",
      icon = icon("users")
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
      
    bubbles(df$n, df$package, key = df$package,color = cx(nrow(df)) )
  })
  
  output$packageTable <- renderTable({
    pkgData() %>%
      group_by(package) %>%
      tally() %>%
      arrange(desc(n), tolower(package)) %>%
      mutate(percentage = n / nrow(pkgData()) * 100) %>%
      select("Web site" = package, "% of activity" = percentage) %>%
      as.data.frame() %>%
      head(15)
  }, digits = 1)
  
  output$downloadCsv <- downloadHandler(
    filename = "cranlog.csv",
    content = function(file) {
      write.csv(pkgData(), file)
    },
    contentType = "text/csv"
  )
  
  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(tail(pkgData(), input$maxrows))
    options(orig)
  })
}

