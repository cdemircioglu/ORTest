# Load the package required to read XML files.
library("XML")

# Also load the other required package.
library("methods")

# Get the arguments
args <- commandArgs(trailingOnly = TRUE)

# Give the input file name to the function.
xmlDoc <- xmlParse(args, asText=TRUE)

parameterName <- xpathSApply(xmlDoc,'//parameter/name',xmlValue)
parameterValue<- xpathSApply(xmlDoc,'//parameter/value',xmlValue)  

print(parameterName)

