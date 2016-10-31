library(colorspace)
library(shiny)
library(shinySignals)
library(dplyr)
library(shinydashboard)
library(bubbles)
source("bloomfilter.R")
  
function(input, output, session) {
  
  #######VARIABLES SECTION#######    
  
  df_duration <- data.frame(FOO=c("12726|ACCESSORIES","11406|ACCOUNTING","5458|ADD","28378|ADULT_EDUCATION","5040|ARTS_CRAFTS","9345|BARBECUES_GRILLING","1436|CHRISTIANITY","25115|EDUCATIONAL_INSTITUTIONS","20463|ENTERTAINMENT_NEWS_CELEBRITY_SITES","13987|ENTERTAINMENT_OTHER","15515|FINANCIAL_PLANNING","7303|HEALTH_LOWFAT_COOKING","20491|INVESTING","21276|LITERATURE_BOOKS","23751|MOVIES","17626|MUSIC","25585|PRIVATE_SCHOOL","4132|PSYCHOLOGY_PSYCHIATRY","24170|REFERENCE_MATERIALS_MAPS","2900|SMOKING_CESSATION","228|SPACE_ASTRONOMY","25213|SPECIAL_EDUCATION","13666|STREAMING_DOWNLOADABLE_VIDEO","9934|TAX_PLANNING","16284|TELEVISION","12826|TEXT_MESSAGING_SMS","16028|WIKIS","30684|YEAR_712_EDUCATION"))  
  df_duration <- data.frame(do.call('rbind', strsplit(as.character(df_duration$FOO),'|',fixed=TRUE)))
  
  timeRequired <- df_duration[which(df_duration$X2 == marketInterest),1] #seconds to complete
  initialtimeRequired <- isolate(timeRequired) #initial time required to calc percent complete
  
  resetfactor <- 0 
  lastservercnt <- 0
  lastmarketInterest <<- "Dummy"
  lastperceivedValue <<- -1
  lastcosttoDeliver <<- -1
      
  # Record the time that the session started.
  startTime <- as.numeric(Sys.time())
  
  # An empty prototype of the data frame we want to create
  prototype <- data.frame(date = character(), time = character(),size = numeric(), r_version = numeric(), r_arch = numeric(),r_os = character(), package = character(), version = character(),country = character(), ip_id = character(), received = numeric())
  
  packageStream <- function(session) {
    # Connect to data source
    #sock <- socketConnection("cransim.rstudio.com", 6789, blocking = FALSE, open = "r")
    #sock <- socketConnection("localhost", 8081, blocking = FALSE, open = "r")
    #sock <- socketConnection(host="localhost", port = 8081, blocking=TRUE,server=FALSE, open="r")
    sock <- socketConnection(host="localhost", port = 8081, blocking=FALSE,server=FALSE, open="r")
    
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
      if (timeRequired < 1)
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
      
      if(resetfactor == 0)
      {
          result = tryCatch({
              rbind(memo, value) %>%
              filter(received > as.numeric(Sys.time()) - timeWindow)
          }, error = function(e) {
              #Insert dummy record to stop the rendering
              new.prototype <- data.frame(date = character(), time = character(),size = numeric(), r_version = numeric(), r_arch = numeric(),r_os = character(), package = character(), version = character(),country = character(), ip_id = character(), received = numeric())
              new.prototype <- data.frame(date = "2016-09-27", time = "07:57:22",size = 9263737, r_version = 1234, r_arch = 500,r_os = "linux-gnu", package = "Loading", version = "1.60.0-2",country = "DE", ip_id = "23657", received = as.numeric(Sys.time())-299)
              rbind(new.prototype, prototype) %>%
              filter(received > as.numeric(Sys.time()) - timeWindow)
              resetfactor <<- 0 #Trip the fuse
            } 
          )
        
      } else
      {
          #Insert dummy record to stop the rendering
          new.prototype <- data.frame(date = character(), time = character(),size = numeric(), r_version = numeric(), r_arch = numeric(),r_os = character(), package = character(), version = character(),country = character(), ip_id = character(), received = numeric())
          new.prototype <- data.frame(date = "2016-09-27", time = "07:57:22",size = 9263737, r_version = 12345, r_arch = 500,r_os = "linux-gnu", package = "Loading", version = "1.60.0-2",country = "DE", ip_id = "23657", received = as.numeric(Sys.time())-299)
          rbind(new.prototype, prototype) %>%
          filter(received > as.numeric(Sys.time()) - timeWindow)
          resetfactor <<- 0 #Trip the fuse
      }
      
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
        
        # Reset the total count
        if (resetfactor != 0)
          total <<- 0
        
        # Add the new IDs so we know for next time
        sapply(ids[newIds], bloomFilter$set)
      }
      total
    })
  }
  
  # Call function
  customerCount <- userCount(pkgStream)
  
  #######OBSERVE PARAMETERS#######
  
  observe({

    # Check the parameters, if they are changed reset the data frame. 
    if (lastmarketInterest != input$marketInterest || lastperceivedValue != input$perceivedValue || lastcosttoDeliver != input$costtoDeliver)
    {
      resetfactor <<- 1 #Reset the data frame
      lastmarketInterest <<- input$marketInterest
      lastperceivedValue <<- input$perceivedValue
      lastcosttoDeliver <<- input$costtoDeliver
      timeRequired <<- df_duration[which(df_duration$X2 == marketInterest),1]
      initialtimeRequired <<- timeRequired
    }

    
    
    

    # We'll use these multiple times, so use short var names for convenience.
    parameterValue <- c(input$servercnt,input$marketInterest,input$perceivedValue,input$costtoDeliver)
    parameterName <- c("servercnt","marketInterest","perceivedValue","costtoDeliver")
    
    # Command start
    cmdString <- '/home/cem/RabbitMQ/send.py "<ShinnyParameters>'
    
    # Build the xml parameters
    for (i in 1:length(parameterValue))
    {
      parameterString <- '<parameter><name>nnn</name><value>vvv</value></parameter>'
      parameterString <- gsub("nnn",parameterName[i],parameterString)
      parameterString <- gsub("vvv",parameterValue[i],parameterString)
      cmdString <- paste(cmdString,parameterString,sep="")
    }
    
    # Command end
    cmdString <- paste(cmdString,'</ShinnyParameters>"',sep="")
    
    # Send the message
    system(cmdString)
    
  })
  
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
    percent <- (1-timeRequired/initialtimeRequired)*100
    if (percent > 100)
      percent <- 100
    
    valueBox(
      value = formatC(percent, digits = 2, format = "f"),
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
    if (timeleft < 0)
      timeleft <- 0 
    
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
      value = customerCount(),
      subtitle = "Customers scanned",
      icon = icon("users")
    )
  })
  
  output$packagePlot <- renderBubbles({
    if (nrow(pkgData()) == 0)
      return()
    
    order <- unique(pkgData()$size)
    df <- pkgData() %>%
      group_by(size) %>%
      summarise( 
        cmsisdn = sum(r_version)
      ) %>%
      arrange(desc(size), tolower(size)) %>%
      # Just show the top 60, otherwise it gets hard to see
      head(60)
    
    bubbles(df$cmsisdn, paste("$",df$size, "/", df$cmsisdn, sep="" ), key = df$size, color = cx(nrow(df)) )
    
  })
  
  output$packageTable <- renderTable({
    if (nrow(pkgData()) == 0)
      return()
    
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
