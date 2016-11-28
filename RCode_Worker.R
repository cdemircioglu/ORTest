#Get the library 
library("DBI")
library("dplyr")
library("methods")
library("data.table")
library("XML")


# Get the arguments
#args <- commandArgs(trailingOnly = TRUE)
#args <- "<ShinnyParameters><parameter><name>servercnt</name><value>3</value></parameter><parameter><name>marketInterest</name><value>INVESTING</value></parameter><parameter><name>perceivedValue</name><value>40</value></parameter><parameter><name>costtoDeliver</name><value>10</value></parameter><parameter><name>runCheck</name><value>2016</value></parameter><parameter><name>runnumber</name><value>20161031102213</value></parameter><parameter><name>runtime</name><value>1000</value></parameter><parameter><name>msisdn</name><value>5348,5599,7143,7461,8583,8717,9447,9453,9639,9937,10042,10166,10327,10385,10570,10746,10749,10900,10930,10940,11013,11288,11413,11460,11537,11549,11654,11682,11920,11943,11996,12003,12033,12055,12096,12105,12379,12380,12446,12643,12676,12720,12808,12862,12899,12917,12985,13037,13053,13142,13147,13214,13337,13342,13346,13390,13407,13420,13501,13521,13555,13677,13684,13734,13787,13788,13800,13855,13870,13901,13906,13935,13953,13965,14101,14125,14141,14145,14153,14227,14240,14291,14352,14385,14390,14422,14435,14447,14495,14566,14708,14714,14806,14868,14875,14931,14941,14944,14975,15006</value></parameter><parameter><name>marketinterestid</name><value>5</value></parameter><parameter><name>sprayprayuptake</name><value>0.5</value></parameter></ShinnyParameters>"
#args <- "<ShinnyParameters><parameter><name>servercnt</name><value>10</value></parameter><parameter><name>marketInterest</name><value>MOVIES</value></parameter><parameter><name>perceivedValue</name><value>40</value></parameter><parameter><name>costtoDeliver</name><value>40</value></parameter><parameter><name>runCheck</name><value>1</value></parameter><parameter><name>runnumber</name><value>20161123055954</value></parameter><parameter><name>runtime</name><value>2144253</value></parameter><parameter><name>msisdn</name><value>03629,303630,303631,303632,303633,303634,303635,303636,303637,303638,303639,303640,303642,303644,303645,303646,303647,303648,303649,303650,303652,303654,303655,303656,303657,303659,303660,303661,303662,303663,303664,303665,303666,303668,303669,303670,303671,303672,303673,303674,303675,303676,303677,303678,303679,303680,303681,303682,303683,303684,303685,303686,303687,303688,303689,303691,303692,303694,303695,303696,303698,303699,303700,303702,303703,303704,303705,303706,303707,303708,303710,303711,303712,303713,303714,303715,303716,303717,303718,303719,303720,303721,303722,303723,303724,303725,303726,303727,303728,303730,303731,303732,303733,303734,303735,303736,303737,303738,303739,303740</value></parameter><parameter><name>marketinterestid</name><value>13</value></parameter><parameter><name>sprayprayuptake</name><value>0.10</value></parameter></ShinnyParameters>"
args <- "RRRR"

# Give the input file name to the function.
xmlDoc <- xmlParse(args, asText=TRUE)

# Read the parameters from xml
parameterName <- xpathSApply(xmlDoc,'//parameter/name',xmlValue)
parameterValue<- xpathSApply(xmlDoc,'//parameter/value',xmlValue)  

# Create the parameters
servercnt <- parameterValue[1]
marketInterest <- parameterValue[2]
perceivedValue <- as.numeric(parameterValue[3])
costtoDeliver <- as.numeric(parameterValue[4])
runCheck <- parameterValue[5]
runnumber <- parameterValue[6]
runTime <- parameterValue[7]
msisdn <- parameterValue[8]
marketInterestID <- parameterValue[9]
sprayprayUptake <- as.numeric(parameterValue[10])


#Create the connection string
con <-  dbConnect(RMySQL::MySQL(),username = "root", password = "KaraburunCe2", host = "localhost", port = 3306, dbname = "openroads")

#Create the query for xdr records
src_query <- ("SELECT MSISDN, CREDITSCORE, HHINCOME, ASSETS, DOWNLOADBYTES FROM fct_marketinterest WHERE MSISDN IN (aaa) AND MARKETINTERESTID = 'bbb'")
src_query <- gsub("aaa",msisdn,src_query)
src_query <- gsub("bbb",marketInterestID,src_query)

#Get the records for xdr
src_xdr <<- dbGetQuery(con, src_query)

#Group the records
src_xdr <- data.table(src_xdr)
src_xdr <- data.frame(src_xdr[,list(CREDITSCORE=mean(CREDITSCORE),HHINCOME=mean(HHINCOME),ASSETS=mean(ASSETS),DOWNLOADBYTES = sum(DOWNLOADBYTES)),by=MSISDN])

#Disconnect from the database
dbDisconnect(con)

####FUNCTIONS####

#Afflunce function 
fun_Afflunce <- function()
{
  #Split the column into multiple columns
  #src_profile <- within(src_profile, PROFILE<-data.frame(do.call('rbind', strsplit(as.character(PROFILE), '|', fixed=TRUE))))
  
  #Calculations
  #x <- (creditScore/860*(assets + 3*houseHoldIncome + 100000))/100000
  
  src_xdr["AFFLUNCE"] <<- src_xdr[,2]/860*(src_xdr[,4]+3*src_xdr[,3])/100000
  src_xdr["RESPONSE"] <<- sqrt(as.numeric(src_xdr$DOWNLOADBYTES))*sqrt(src_xdr$AFFLUNCE)*perceivedValue*sprayprayUptake/1000
  #src_xdr["ECONOMICBENEFIT"] <<- (perceivedValue*src_xdr["RESPONSE"])/max(src_xdr["RESPONSE"]*1.2) - costtoDeliver
  src_xdr["ECONOMICBENEFIT"] <<- (perceivedValue*1.8*src_xdr["RESPONSE"])/abs(max(src_xdr["RESPONSE"])) - costtoDeliver
}

#Run the monte carlo 
fun_MC <- function()
{
  for (n in src_xdr$ECONOMICBENEFIT) {
    randomVector <- sample.int(2, size = 100, replace = TRUE, prob = NULL)-1
    #tresult <- sapply(1:(length(randomVector)), function(j) trunc(sum(n*randomVector[j]),prec = 2))
    tresult <- sapply(1:(length(randomVector)), function(j) sum(n*randomVector[j]))
    fresult <<- fresult + tresult
  }
}



####CALL FUNCTIONS####
fun_Afflunce()
#fun_Response()
#fun_EconomicBenefit()

#Reset the factor
fresult <<- seq(1,100)*0
fun_MC()

#Create the buckets
buckets <- seq(-50, 300, by=2)

#Create the bucketing logic
finalset <- transform(src_xdr, LABEL=cut(ECONOMICBENEFIT,breaks=buckets,labels=buckets[1:175]))
finalset <- finalset[complete.cases(finalset),] #Remove na figures, if any

#Calculate the counts
finalset <- finalset %>%
  group_by(LABEL) %>% 
  summarise_each(funs(n()),MSISDN)

#Per customer 
mcValue <- paste(2*round(fresult/nrow(src_xdr),digits=0),collapse=" ")

#Create the dataset
finalset <- as.data.frame(finalset)
finalset["HOST"] <- "na.com"
finalset <- finalset[complete.cases(finalset),] #Remove na figures, if any
#finalset["MCValue"] <- mcValue


#Create the arguments string
arg <- ""

#Loop through 
for (row in 1:nrow(finalset)) {
  ####
  # Output string
  output <- 'aaa,bbb,ccc,ddd,eee,fff'
  output <- gsub("aaa",runnumber,output) #This is the run number
  output <- gsub("bbb",finalset[row,1],output) #Bucket
  output <- gsub("ccc",finalset[row,2],output) #Count
  output <- gsub("ddd",runTime,output) #Runtime
  output <- gsub("eee",finalset[row,3],output) #Website
  output <- gsub("fff",runCheck,output) #RunCheck
  #output <- gsub("07:57:22",format(Sys.time(), format = "%H:%M:%S"),output) #Repace the time with actual time
  arg <- paste(arg,output,sep="|")
}

#Recast the data
output <- substring(arg, 2) # Remove the first pipe

#Command start
cmdString <- paste("python send.py '", paste(mcValue,runCheck,output,sep="_MM_"),"'", sep="")

#Send the message
system(cmdString)
