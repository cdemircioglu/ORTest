library(colorspace)
library(shiny)
library(shinySignals)
library(dplyr)
library(shinydashboard)
library(bubbles)
source("bloomfilter.R")
  
function(input, output, session) {
  
  #######VARIABLES SECTION#######    
  
  df_duration <- data.frame(FOO=c("127260|ACCESSORIES","114060|ACCOUNTING","54580|ADD","283780|ADULT_EDUCATION","50400|ARTS_CRAFTS","93450|BARBECUES_GRILLING","14360|CHRISTIANITY","251150|EDUCATIONAL_INSTITUTIONS","204630|ENTERTAINMENT_NEWS_CELEBRITY_SITES","139870|ENTERTAINMENT_OTHER","155150|FINANCIAL_PLANNING","73030|HEALTH_LOWFAT_COOKING","204910|INVESTING","212760|LITERATURE_BOOKS","237510|MOVIES","176260|MUSIC","255850|PRIVATE_SCHOOL","41320|PSYCHOLOGY_PSYCHIATRY","241700|REFERENCE_MATERIALS_MAPS","29000|SMOKING_CESSATION","22800|SPACE_ASTRONOMY","252130|SPECIAL_EDUCATION","136660|STREAMING_DOWNLOADABLE_VIDEO","99340|TAX_PLANNING","162840|TELEVISION","128260|TEXT_MESSAGING_SMS","160280|WIKIS","306840|YEAR_712_EDUCATION"))  
  df_duration <- data.frame(do.call('rbind', strsplit(as.character(df_duration$FOO),'|',fixed=TRUE)))
  runCheck <- 1
    
  #timeRequired <- df_duration[which(df_duration$X2 == marketInterest),1] #seconds to complete
  timeRequired <- 7500 #seconds to complete
  initialtimeRequired <- isolate(timeRequired) #initial time required to calc percent complete
  
  resetfactor <- 1 
  lastservercnt <- 0
  total <- 0
  lastmarketInterest <<- "Dummy"
  lastperceivedValue <<- -1
  lastcosttoDeliver <<- -1
      
  # Record the time that the session started.
  startTime <- as.numeric(Sys.time())
  
  # An empty prototype of the data frame we want to create
  prototype <- data.frame(date = character(), time = character(),size = numeric(), r_version = numeric(), r_arch = numeric(),r_os = numeric(), package = character(), version = character(),country = character(), runCheckDF = numeric(), received = numeric())
  
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
      invalidateLater(2000/ifelse(input$servercnt==0,0.01,input$servercnt), session)
      readLines(sock)
    })
    
    # Parses newLines() into data frame
    reactive({
      if (length(newLines()) == 0)
        return()
      if (timeRequired < 1)
        return()
      
      #Read the stream
      strcon <- textConnection(newLines())
      str <- readLines(strcon)
      str <- unlist(str)
      qstr <- ""
      
      #This is the check for MC
      if(grepl("_MM_", str)) {
        str <- unlist(str)
        str <- strsplit(as.character(str), split="_MM_")
        str <- unlist(str)
        
        #Hold on to the MC values
        mcv <- as.numeric(unlist(strsplit(str[1], split=" ")))
      
        #Get the runCheck
        currentrunCheck <- as.numeric(str[2])
        
        #Check the runCheck if it is greater
        if (currentrunCheck > runCheck)
        {
          runCheck <<- currentrunCheck #Assign the current check
          
          #Delete the data frame
          if(exists("mcv_df"))
          {
            mcv_df <<- mcv_df[0,]
          }
          #return()
        }
        
        #Check the runCheck if it is greater
        if (currentrunCheck != runCheck)
        {
          #return()
        }
        
        #Create the data frame
        if(exists("mcv_df"))
        {
          mcv_df <<- rbind(mcv_df,data.frame(mcv))
        } else {
          mcv_df <<- data.frame(mcv)
        }
        
        qstr <- paste(str[-1],collapse="\n")
      
      } else {
        qstr <- str
      }
      
      
      #read.csv(textConnection(newLines()), header=FALSE, stringsAsFactors=FALSE,
      read.csv(text=qstr, header=FALSE, stringsAsFactors=FALSE,
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
              filter(runCheckDF == runCheck)
          }, error = function(e) {
              #Insert dummy record to stop the rendering
              new.prototype <- data.frame(date = character(), time = character(),size = numeric(), r_version = numeric(), r_arch = numeric(),r_os = numeric(), package = character(), version = character(),country = character(), runCheckDF = numeric(), received = numeric())
              new.prototype <- data.frame(date = "2016-09-27", time = "07:57:22",size = 9263737, r_version = 1234, r_arch = 500,r_os = 64, package = "Loading", version = "1.60.0-2",country = "DE", runCheckDF = 23657, received = as.numeric(Sys.time())-299)
              rbind(new.prototype, prototype) %>%
              filter(runCheckDF == runCheck)
              resetfactor <<- 0 #Trip the fuse
            } 
          )
        
      } else
      {
          #Insert dummy record to stop the rendering
          new.prototype <- data.frame(date = character(), time = character(),size = numeric(), r_version = numeric(), r_arch = numeric(),r_os = numeric(), package = character(), version = character(),country = character(), runCheckDF = numeric(), received = numeric())
          new.prototype <- data.frame(date = "2016-09-27", time = "07:57:22",size = 9263737, r_version = 12345, r_arch = 500,r_os = 64, package = "Loading", version = "1.60.0-2",country = "DE", runCheckDF = 23657, received = as.numeric(Sys.time())-299)
          rbind(new.prototype, prototype) %>%
          filter(received > as.numeric(Sys.time()) - timeWindow)
          resetfactor <<- 0 #Trip the fuse
      }
      
    }, prototype)
  }
  
  
  
  
  # Set the stream of session
  pkgStream <- packageStream(session)
  
  # Max age of data (5 minutes)
  maxAgeSecs <- 60 * 500
  
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
    #total <- 0
    reactive({
      df <- pkgStream()
      if (!is.null(df) && nrow(df) > 0) {
        ## runCheck is only unique on a per-day basis. To make them unique
        ## across days, include the date. And call unique() to make sure
        ## we don't double-count dupes in the current data frame.
        #ids <- paste(df$date, df$runCheck) %>% unique()
        ## Get indices of IDs we haven't seen before
        #newIds <- !sapply(ids, bloomFilter$has)
        ## Add the count of new IDs
        #total <<- total + length(newIds)
        #total <<- 500 ##total + sum(df$r_version)
        # Reset the total count
        if (resetfactor != 0)
          total <<- 0
        
        
        # Add the new IDs so we know for next time
        #sapply(ids[newIds], bloomFilter$set)
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
      currentMarketInterest <- df_duration[which(df_duration$X2 == input$marketInterest),]
      timeRequired <<- as.numeric(as.character(currentMarketInterest[1]))*1028
      initialtimeRequired <<- timeRequired
      runCheck <<- as.numeric(as.character(Sys.time(),format="%H%M%S"))
      #Delete the data frame
      if(exists("mcv_df"))
      {
        mcv_df <<- mcv_df[0,]
      }
      
    }

    
    
    

    # We'll use these multiple times, so use short var names for convenience.
    parameterValue <- c(input$servercnt,input$marketInterest,input$perceivedValue,input$costtoDeliver,runCheck)
    parameterName <- c("servercnt","marketInterest","perceivedValue","costtoDeliver","runCheck")
    
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
  
  cn <- function (n, h = c(360, 293), c. = c(80, 26), l = c(34, 95), power = c(0.7, 1.3), fixup = TRUE, gamma = NULL, alpha = 1,...) 
  {
    if (!is.null(gamma)) 
      warning("'gamma' is deprecated and has no effect")
    if (n < 1L) 
      return(character(0L))
    h <- rep(h, length.out = 2L)
    c <- rep(c., length.out = 2L)
    l <- rep(l, length.out = 2L)
    power <- rep(power, length.out = 2L)
    rval <- seq(1, 0, length = n)
    rval <- hex(polarLUV(L = l[2L] - diff(l) * rval^power[2L], 
                         C = c[2L] - diff(c) * rval^power[1L], H = h[2L] - diff(h) * 
                           rval), fixup = fixup, ...)
    if (!missing(alpha)) {
      alpha <- pmax(pmin(alpha, 1), 0)
      alpha <- format(as.hexmode(round(alpha * 255 + 1e-04)), 
                      width = 2L, upper.case = TRUE)
      rval <- paste(rval, alpha, sep = "")
    }
    return(rval)
  }
  
  
  cp <- function (n, h = c(130, 30), c. = c(65, 0), l = c(45, 90), power = c(0.5, 1.5), fixup = TRUE, gamma = NULL, alpha = 1, ...) 
  {
    if (!is.null(gamma)) 
      warning("'gamma' is deprecated and has no effect")
    if (n < 1L) 
      return(character(0L))
    h <- rep(h, length.out = 2L)
    c <- rep(c., length.out = 2L)
    l <- rep(l, length.out = 2L)
    power <- rep(power, length.out = 2L)
    rval <- seq(1, 0, length = n)
    rval <- hex(polarLUV(L = l[2L] - diff(l) * rval^power[2L], 
                         C = c[2L] - diff(c) * rval^power[1L], H = h[2L] - diff(h) * 
                           rval), fixup = fixup, ...)
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
    invalidateLater(1000, session) 
    percent <- (1-timeRequired/initialtimeRequired)*100
    if (percent > 100)
      percent <- 100
    
    valueBox(
      #value = customerCount(),
      value = prettyNum(paste(floor(percent*100),"K",sep=""), scientific=FALSE, big.mark=','),
      subtitle = "Customers scanned",
      icon = icon("users")
    )
  })

  output$totalcustomersScanned <- renderValueBox({
    invalidateLater(1000, session) 
    df <- pkgData() %>%
      summarise( 
        cmsisdn = sum(r_version)
      )
    
    if(exists("df"))
    {
      valueBox(
        #value = customerCount(),
        value = paste(prettyNum(floor(df$cmsisdn/500), scientific=FALSE, big.mark=','),"K",sep=""),
        #value = df$cmsisdn,
        subtitle = "Customers within the market",
        icon = icon("users")
      )
    } else {
      valueBox(
        #value = customerCount(),
        value = paste(prettyNum(0, scientific=FALSE, big.mark=','),"K",sep=""),
        #value = df$cmsisdn,
        subtitle = "Customers within the market",
        icon = icon("users")
      )
    } 
    
    
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
      head(50)
      total <<- sum(df$cmsisdn)      
      #bubbles(df$cmsisdn, paste("$",df$size, "/", df$cmsisdn, sep="" ), key = df$size, color = cx(nrow(df)) )
      bubbles(df$cmsisdn, paste("$",((df$size/4)-input$costtoDeliver), "/", df$cmsisdn/100,"K",sep="" ), key = df$size, color = c(cp(nrow(df[which(floor((df$size/4)-input$costtoDeliver)>=0),])),rev(cn(nrow(df[which(floor((df$size/4)-input$costtoDeliver)<0),])))) )
      
  })
  
  output$plot <- renderPlot({
  
    tcol="orange"      # fill colors
    acol="orangered"   # color for added samples
    tscale=1;          # label rescaling factor
    df <- pkgData()
      
    
    hist(mcv_df$mcv*((input$perceivedValue*1.011-input$costtoDeliver)/50), 
         warn.unused = FALSE,
         #breaks=21,
         main="",
         col=tcol,         
         ylim=c(0,5000),
         ylab="Frequency (1K)",
         border=tcol,
         xlab="Revenue",
         cex.lab=tscale,
         cex.axis=tscale,
         cex.main=tscale,
         cex.sub=tscale
    )
    
    
      
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
      head(10)
  }, digits = 1)
  
  
  
  
  
}
