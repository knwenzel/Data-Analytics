#### Kelsey Wenzel
#### IST 687
#### Homework 6
#### Description: Viz HW: Air Quality Analysis

#install.packages("ggplot2")
library(ggplot2)
library(reshape2)

## Step 1: Load the data
airdf<- airquality

## Step 2: Clean the data
# Replace null values with mean of column
airdf$Solar.R[is.na(airdf$Solar.R)] <- mean(airdf$Solar.R[!is.na(airdf$Solar.R)])
airdf$Ozone[is.na(airdf$Ozone)] <- mean(airdf$Ozone[!is.na(airdf$Ozone)])
airdf$Wind[is.na(airdf$Wind)] <- mean(airdf$Wind[!is.na(airdf$Wind)])
airdf$Temp[is.na(airdf$Temp)] <- mean(airdf$Temp[!is.na(airdf$Temp)])


## Step 3: Understand the Data Distribution
# create histogram for each variable
ozonehist<- ggplot(airdf, aes(x=Ozone)) + geom_histogram(bins=10, fill="white",color="black") + ggtitle("Ozone Buckets")
ozonehist
solarhist<- ggplot(airdf, aes(x=Solar.R)) + geom_histogram(bins=10, fill="white",color="black") + ggtitle("Solar.R Buckets")
solarhist
windhist<- ggplot(airdf, aes(x=Wind)) + geom_histogram(bins=5, fill="white",color="black") + ggtitle("Wind Buckets")
windhist
temphist<- ggplot(airdf, aes(x=Temp)) + geom_histogram(bins=5, fill="white",color="black") + ggtitle("Temp Buckets")
temphist
monthhist<- ggplot(airdf, aes(x=Month)) + geom_histogram(bins=5, fill="white",color="black") + ggtitle("Month Buckets")
monthhist
dayhist<- ggplot(airdf, aes(x=Day)) + geom_histogram(bins=5, fill="white",color="black") +ggtitle ("Day Buckets")
dayhist

# Boxplot for Ozone
ozonebox <- ggplot(airdf,aes(x=factor(0),Ozone)) + geom_boxplot() + ggtitle ("Ozone Boxplot")
ozonebox

# Boxplot for Wind with rounding wind values
windbox <- ggplot(airdf,aes(x=factor(0),round(Wind))) + geom_boxplot() + ggtitle("Wind Boxplot")
windbox

## Step 4: Explore how the data changes over time
# create date column
newdate <- paste(airdf$Month, airdf$Day, "1973", sep="/", collapse = NULL)
airdf$date <- as.Date(newdate, "%m/%d/%Y")

# Create line charts for Ozone, Solar.R, Wind and Temp
ozoneline <- ggplot(airdf, aes(x=date, y=Ozone)) + geom_line (color="red",size=2.0)
ozoneline
solarline <- ggplot(airdf, aes(x=date, y=Solar.R)) + geom_line (color="blue", size=2.0)
solarline
windline <- ggplot(airdf,aes(x=date, y=Wind)) + geom_line(color="black", size=2.0)
windline
templine <- ggplot(airdf, aes(x=date, y=Temp)) +geom_line(color="green", size=2.0)
templine

# Create a line chart with all 4 Variables
allfour<- ggplot(airdf, aes(x=date)) +
  geom_line(aes(y=Ozone,col="Ozone"),size=1) +
  geom_line(aes(y=Solar.R,col="Solar.R"),size=1) +
  geom_line(aes(y=Wind,col="Wind"),size=1) +
  geom_line(aes(y=Temp,col="Temp"),size=1) +
  guides(col=guide_legend("Variable"))+
  ylab("Measure")  +
  scale_y_continuous(breaks=seq(0,350,20))
allfour

## Step 5: Look at all the data via a Heatmap
# melt data to get x,y,and z values for heatmap
melteddf<- melt(data = airdf, id.vars = "date", measure.vars = c("Ozone", "Solar.R", "Wind", "Temp"))

# create a heatmap
heatgrey<- ggplot(melteddf, aes(x=date, y=variable)) +
  geom_tile(aes(fill= value), color="white") +scale_fill_gradient(low="white",high="black")
heatgrey

## Step 6: Look at al the data via a scatter chart
scatter <- ggplot(airdf,aes(x=Wind,y=Temp)) + geom_point(aes(size=Ozone, color=Solar.R))
scatter

## Step 7: Final Analysis 
# Do you see any patterns after exploring the data?:
# I noticed different patterns with each visual. With the Line graphs containing all four variables, I was 
# able to notice how Solar.R's range was much greater than the other variables and how Wind and Temperate were
# more stable with range. With the scatter chart, you are able to see different relationships among the variables,
# such as apparent correlations among Wind, Temp and Ozone.

# What was the most useful Visualization?
# I personally thought the scatter plot was the most useful for seeing relationships. However, it would depend
# on the questions asked about the data. For simple math questions, such as max or minimum, the line graph with
# all four variables would be the most useful. 



