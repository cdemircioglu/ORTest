library(colorspace)
library(shiny)
library(shinySignals)
library(dplyr)
library(shinydashboard)
library(bubbles)
source("bloomfilter.R")
  
function(input, output, session) {
  
  #######VARIABLES SECTION#######    
  
  timeRequired <- 8000 #seconds to complete
  initialtimeRequired <- isolate(timeRequired) #initial time required to calc percent complete
  
  # Record the time that the session started.
  startTime <- as.numeric(Sys.time())
  
  # An empty prototype of the data frame we want to create
  prototype <- data.frame(date = character(), time = character(),size = numeric(), r_version = character(), r_arch = character(),r_os = character(), package = character(), version = character(),country = character(), ip_id = character(), received = numeric())
  
  packageStream <- function(session) {
    # Connect to data source
    sock <- socketConnection("cransim.rstudio.com", 6789, blocking = FALSE, open = "r")
    # Clean up when session is over
    session$onSessionEnded(function() {
      close(sock)
    })
    
    # Returns new lines
    newLines <- reactive({
      invalidateLater(1000, session)
      readLines(sock)
    })
    
    # Parses newLines() into data frame
    reactive({
      if (length(newLines()) == 0)
        return()
      read.csv(textConnection(newLines()), header=FALSE, stringsAsFactors=FALSE,
               col.names = names(prototype)
      ) %>% mutate(received = as.numeric(Sys.time()))
    })
  }
  
  # Accumulates pkgStream rows over time; throws out any older than timeWindow
  # (assuming the presence of a "received" field)
  packageData <- function(pkgStream, timeWindow) {
    shinySignals::reducePast(pkgStream, function(memo, value) {
      rbind(memo, value) %>%
        filter(received > as.numeric(Sys.time()) - timeWindow)
    }, prototype)
  }
  
  
  # Set the stream of session
  pkgStream <- packageStream(session)
  
  # Max age of data (5 minutes)
  maxAgeSecs <- 60 * 5
  
  # Set package
  pkgData <- packageData(pkgStream, maxAgeSecs)
  
  # Use a bloom filter to probabilistically track the number of unique
  # users we have seen; using bloom filter means we will not have a
  # perfectly accurate count, but the memory usage will be bounded.
  userCount <- function(pkgStream) {
    # These parameters estimate that with 5000 unique users added to
    # the filter, we'll have a 1% chance of false positive on the next
    # user to be queried.
    bloomFilter <- BloomFilter$new(5000, 0.01)
    total <- 0
    reactive({
      df <- pkgStream()
      if (!is.null(df) && nrow(df) > 0) {
        # ip_id is only unique on a per-day basis. To make them unique
        # across days, include the date. And call unique() to make sure
        # we don't double-count dupes in the current data frame.
        ids <- paste(df$date, df$ip_id) %>% unique()
        # Get indices of IDs we haven't seen before
        newIds <- !sapply(ids, bloomFilter$has)
        # Add the count of new IDs
        total <<- total + length(newIds)
        # Add the new IDs so we know for next time
        sapply(ids[newIds], bloomFilter$set)
      }
      total
    })
  }
  
  # Call function
  customerCount <- userCount(pkgStream)
  
  # Color function
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
  
  
  #######OUTPUT SECTION#######    
  
  output$completePercent <- renderValueBox({
    invalidateLater(1000, session) 
    valueBox(
      value = formatC((1-timeRequired/initialtimeRequired)*100, digits = 2, format = "f"),
      subtitle = "Percent completed",
      icon = icon("percent"),
      color = "yellow" 
    )
  })
  
  output$timetoComplete <- renderValueBox({
    invalidateLater(1000, session) 
    elapsed <- as.numeric(Sys.time()) - startTime #Assumes a constant value to complete the job
    timeRequired <<- timeRequired - input$servercnt
    timeleft <- timeRequired/(input$servercnt)
    
    valueBox(
      value = format(as.POSIXct('2016-01-01 00:00:00') + timeleft, "%H:%M:%S"),
      subtitle = "Time to complete",
      icon = icon("clock-o")
    )
  })
  
  output$costPerHour <- renderValueBox({
    invalidateLater(1000, session) 
    costRate <- input$servercnt*1.50 #Based on Azure pricing calculator, does not include storage
    
    valueBox(
      value =  formatC(costRate, digits = 2, format = "f"),
      subtitle = "Cost per hour",
      icon = icon("usd")
    )
  })
  
  output$customersScanned <- renderValueBox({
    
    valueBox(
      value =  customerCount(),
      subtitle = "Customers scanned",
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
  
  
  
  
  
}
