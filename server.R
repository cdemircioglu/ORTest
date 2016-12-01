function(input, output, session) {
  
  #######VARIABLES SECTION#######    
  df_duration <- data.frame(FOO=c("414060|ACCESSORIES","354580|ACCOUNTING","250400|ARTS","293450|ASTRONOMY","451150|CHRISTIANITY","664630|EDUCATION","439870|ENTERTAINMENT","473030|FINANCE","304910|HEALTH","412760|INVESTING","4237510|MOVIES","4037510|MUSIC","4637510|SPORTS","4437510|TECHNOLOGY","4737510|TELEVISION"))  
  df_duration <- data.frame(do.call('rbind', strsplit(as.character(df_duration$FOO),'|',fixed=TRUE)))
  runCheck <- as.numeric(as.character(Sys.time(),format="%H%M%S"))
    
  #timeRequired <- df_duration[which(df_duration$X2 == marketInterest),1] #seconds to complete
  timeRequired <- 7500 #seconds to complete
  initialtimeRequired <- isolate(timeRequired) #initial time required to calc percent complete
  
  resetfactor <- 1 
  lastservercnt <- 0
  total <- 0
  lastmarketInterest <<- "Dummy"
  lastperceivedValue <<- -1
  lastcosttoDeliver <<- -1

  #Delete the data frame
  if(exists("mcv_df"))
  {
    if(is.data.frame(mcv_df))
      try(mcv_df <<- mcv_df[0,])
  }
  
        
  # Record the time that the session started.
  startTime <- as.numeric(Sys.time())
  
  # An empty prototype of the data frame we want to create
  prototype <- data.frame(runnumber = numeric(),bucket = numeric(),rcount = numeric(),runtime = numeric(),website = character(),runcheck = numeric(), received = numeric())
  
  packageStream <- function(session) {
    # Connect to data source
    sock <- socketConnection(host="hwcontrol.cloudapp.net", port = 8091, blocking=FALSE,server=FALSE, open="r", timeout=10000)
    
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
      #print("str begin")
      #This is the check for MC
      if(grepl("_MM_", str)) {
        str <- unlist(str)
        str <- strsplit(as.character(str), split="_MM_")
        str <- unlist(str)
        
        #Hold on to the MC values
        mcv <- as.numeric(unlist(strsplit(str[1], split=" ")))
        
        
        #Get the runCheck
        currentrunCheck <- as.numeric(str[2])
        
        #print(paste("Current run check is ", currentrunCheck, "  and runcheck is ", runCheck,sep=""))
        #Check the runCheck if it is greater
        if (currentrunCheck > runCheck)
        {
          runCheck <<- currentrunCheck #Assign the current check
          
          #Delete the data frame
          if(exists("mcv_df"))
          {
            if(is.data.frame(mcv_df))
              try(mcv_df <<- mcv_df[0,])
          }
          return()
        }
        
        if (currentrunCheck != runCheck) #Wait until the new runcheck is valid. 
        {
          return()
        }
                
        #Create the data frame
        if(exists("mcv_df"))
        {
          mcv_df <<- rbind(mcv_df,data.frame(mcv))
        } else {
          mcv_df <<- data.frame(mcv)
        }
        
        
        qstr <- paste(str[-c(1,2)],collapse="\n")
      
      } else {
        qstr <- str
      }
      
      #print("str end")
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
              filter(runCheck == runcheck)
          }, error = function(e) {
              #Insert dummy record to stop the rendering
              new.prototype <- data.frame(runnumber = numeric(),bucket = numeric(),rcount = numeric(),runtime = numeric(),website = character(),runcheck = numeric(), received = numeric())
              new.prototype <- data.frame(runnumber = 1,bucket = 1,rcount = 1,runtime = 1,website = "na.com",runcheck = 1,received = as.numeric(Sys.time())-299)
              rbind(new.prototype, prototype) %>%
              filter(runCheck == runcheck)
              resetfactor <<- 0 #Trip the fuse
            } 
          )
        
      } else
      {
          #Insert dummy record to stop the rendering
          new.prototype <- data.frame(runnumber = numeric(),bucket = numeric(),rcount = numeric(),runtime = numeric(),website = character(),runcheck = numeric(), received = numeric())
          new.prototype <- data.frame(runnumber = 1,bucket = 1,rcount = 1,runtime = 1,website = "na.com",runcheck = 1,received = as.numeric(Sys.time())-299)
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
        if(is.data.frame(mcv_df))
          try(mcv_df <<- mcv_df[0,])
      }
      
    }

    # We'll use these multiple times, so use short var names for convenience.
    parameterValue <- c(input$servercnt,input$marketInterest,input$perceivedValue,input$costtoDeliver,runCheck)
    parameterName <- c("servercnt","marketInterest","perceivedValue","costtoDeliver","runCheck")
    
    # Command start
    #cmdString <- '/home/cem/ui/send.py "<ShinnyParameters>'
    cmdString <- '"<ShinnyParameters>'
    
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
    
    if(length(grep("C:",getwd()))>0) 
      cmdString <- paste('c:\\Python27\\python.exe send.py ', cmdString, sep="")
    else 
      cmdString <- paste('python /home/cem/ui/send.py ', cmdString, sep="")
    
    #print(cmdString)
    # Send the message
    system(cmdString)
    
  })
  

    
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
        cmsisdn = sum(rcount)
      )
    
    if(exists("df"))
    {
        valueBox(
        #value = customerCount(),
        value = paste(prettyNum(floor(df$cmsisdn/400), scientific=FALSE, big.mark=','),"K",sep=""),
        #value = df$cmsisdn,
        subtitle = "Within the market",
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
    
    dc <- pkgData()
    
    
    if (!is.data.frame(dc)) #If no data found, display loading. 
    {
        bubbles(value = runif(1), label = "Loading", color="white")
       #return()
    }
    else
    {
      # Write CSV in R
      #write.csv(pkgData(), file = "MyData.csv")
      
      order <- unique(pkgData()$bucket)
      df<-pkgData() %>%
        group_by(bucket) %>%
         summarise( 
            cmsisdn = sum(rcount)
         ) %>%
        arrange(desc(bucket), tolower(bucket)) %>%
        # Just show the top 60, otherwise it gets hard to see
        head(50)
        total <<- sum(df$cmsisdn)     
        ####bubbles(df$cmsisdn, paste("$",df$size, "/", df$cmsisdn, sep="" ), key = df$size, color = cx(nrow(df)) )
        bubbles(df$cmsisdn, paste("$",df$bucket, "/", format(round(df$cmsisdn/1000,2), nsmall = 2),"K",sep="" ),         key = df$bucket, color = c(cp(nrow(df[which(floor((df$bucket))>=0),])),rev(cn(nrow(df[which(floor((df$bucket))<0),])))) 
        )
    } 
    
  })
  
  
  output$plot <- renderPlot({
  
    tcol="orange"      # fill colors
    acol="orangered"   # color for added samples
    tscale=1;          # label rescaling factor
    #df <- pkgData()
    invalidateLater(500, session) 
      
    #hist(mcv_df$mcv*((input$perceivedValue*1.011-input$costtoDeliver*1.32)/50),
    hist(mcv_df$mcv, 
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

  
  output$plotMarketInterest <- renderPlot({
    invalidateLater(1000, session) 
    
    con <-  dbConnect(RMySQL::MySQL(),username = "root", password = "KaraburunCe2", host = "hwcontrol.cloudapp.net", port = 3306, dbname = "openroads")
    
    #Create the query for xdr records
    src_query <- ("SELECT B.MARKETINTEREST, A.MARKETCOUNT FROM dim_marketinterest B INNER JOIN (SELECT MARKETINTERESTID, SUM(MARKETCOUNT) AS MARKETCOUNT FROM fct_marketinterestgroup GROUP BY MARKETINTERESTID) A ON A.MARKETINTERESTID = B.MARKETINTERESTID ORDER BY B.MARKETINTEREST DESC")
    
    #Get the records for xdr
    src_xdr <- dbGetQuery(con, src_query)
    
    #Disconnect from the database
    dbDisconnect(con)
    
    bp <- ggplot(src_xdr, aes(x=src_xdr$MARKETINTEREST, y=src_xdr$MARKETCOUNT/1000)) +
      geom_bar(stat="identity",fill="orange") +
      coord_flip() +
      labs(x='Market Interest',y='Activity Count (in thousands)')
    print(bp)
    
  })    
#  output$packageTable <- renderTable({
#    if (nrow(pkgData()) == 0)
#      return()
#    
#    pkgData() %>%
#      group_by(package) %>%
#      tally() %>%
#      arrange(desc(n), tolower(package)) %>%
#      mutate(percentage = n / nrow(pkgData()) * 100) %>%
#      select("Web site" = package, "% of activity" = percentage) %>%
#      as.data.frame() %>%
#      head(10)
#  })
}
