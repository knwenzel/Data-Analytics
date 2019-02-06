#### Kelsey Wenzel
#### IST 687
#### Homework 3
#### Description: Cleaning/munging	Dataframes

##### Step 1:Create a function (named readStates) to read a CSV file into R
readStates <- function (urlToRead) {
  #read the data from the web
  testFrame <- read.csv(url(urlToRead),header = FALSE,stringsAsFactors= TRUE)
  return(testFrame)
}
newdf <- readStates("http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv") 
##### Step	2:	Clean	the	dataframe
# deleting uneccessary rows 
statesData <- newdf[-c(1,2,3,4,5,6,7,8,9,61,62,63,64,65,66,67,68),]
# dropping columns V6-V10
colnames(statesData)
statesData <- subset(statesData, select = -c(V6,V7,V8,V9,V10))
# Renaming Columns
colnames(statesData) <- c("stateName", "base2010",	"base2011", "Jul2010", "Jul2011")
# resetting the index
row.names(statesData) <- NULL
#remove the dot from the state name
statesData$stateName <-gsub("\\.","", statesData$stateName)
# function "makeNumber" to convert factors to numbers
makeNumbers <- function(inputVector)
{
  #get ride of commas
  inputVector <-gsub(",","",inputVector)
  #get ride of spaces
  inputVector <-gsub(" ","", inputVector)
  
  return(as.numeric(inputVector))
}
# calling the function for each column
statesData$base2010 <- makeNumbers(statesData$base2010)
statesData$base2011 <- makeNumbers(statesData$base2011)
statesData$Jul2010 <- makeNumbers(statesData$Jul2010)
statesData$Jul2011 <- makeNumbers(statesData$Jul2011)
## Step	3:	Store	and	Explore	the	dataset
# 6. storing dataset into dataframe as "dfStates"
dfStates <- statesData
# 7. 
mean(dfStates$Jul2011)
## Step 4: Find the state with the Highest Population	
# 8. find highest population state based on July 2011
dfStates[which.max(dfStates$Jul2011),]
# 9. Sort data by increasing values on July 2011 data
orderedJul2011 <- dfStates [order(dfStates$Jul2011),]
orderedJul2011
## Step 5: Explore the distribution of the states
# 10. 
mathStuff <- function (vector, number)
{ 
  lengthOne <- length(vector)
  newVector <- vector[which(vector < number)]
  lengthTwo <- length(newVector)
  probability <- lengthTwo/lengthOne
  return(probability)
}

mathStuff(dfStates$Jul2011,6109645)


