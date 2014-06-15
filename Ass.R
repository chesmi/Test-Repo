######## Loading and preprocessing the data

###1. Load the data (i.e. read.csv())
amData <- read.csv( "activity.csv" )

###2. Transfer date into correct date format remove nas
amData$date <- as.Date( amData$date )

######## What is mean total number of steps taken per day?

### ignore the missing values in the dataset
amNoNA <- amData[complete.cases (amData ), ]

###1. Make a histogram of the total number of steps taken each day
amStepsNoNA <- tapply( amNoNA$steps, amNoNA$date, sum )
hist( amStepsNoNA, main = "Total number of steps taken each day", xlab = "", col = "blue")

###2. Calculate and report the mean and median total number of steps taken per day
mean( amStepsNoNA )
median( amStepsNoNA )

#### What is the average daily activity pattern?

###1. TS plot
amAvgSteps <- tapply( amNoNA$steps, amNoNA$interval, mean )
amIntervals <- as.numeric( unlist( attributes( amAvgSteps )[2]) )
plot( amIntervals, amAvgSteps, type = "l", col = "blue", xlab = "", ylab = "")
title( main = "Average number of steps vs 5-minute interval" )
title( xlab = "5-minute interval" )
title( ylab = "Average number of steps" )

###2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
sum( as.numeric( amAvgSteps == max( amAvgSteps ) ) * amIntervals )

##### Imputing missing values

### 1. Count NA
dim( amData )[1] - dim( amNoNA )[1]

### new dats set
amMedianSteps <- tapply( amNoNA$steps, amNoNA$interval, median )
amIntervalLookup <- data.frame( amMedianSteps, amIntervals )
names( amIntervalLookup )<-c( "MedianSteps", "interval" )
amNew <- ( merge( amIntervalLookup, amData, by = 'interval' ) )
amNew$MedianSteps <- is.na(amNew$steps) * amNew$MedianSteps
amNew[is.na( amNew )] <- 0
amNew2 <- data.frame( amNew$interval, amNew$date, amNew$MedianSteps + amNew$steps )
names( amNew2 )<-c( "interval", "date", "steps" )


### Histogram and mean, medain
amSteps <- tapply( amNew2$steps, amNew2$date, sum )
hist( amSteps, main = "Total number of steps taken each day", xlab = "", col = "blue")

###2. Calculate and report the mean and median total number of steps taken per day
mean( amSteps )
median( amSteps )



### Are there differences in activity patterns between weekdays and weekends?

day <- weekdays( amNew2$date )
day <-( day == "Sunday" | day == "Saturday") * 1
day <- factor( day, labels = c( "weekday", "weekend" ) )

wd <- subset( amNew2 , day == 'weekday' )
we <- subset( amNew2 , day == 'weekend' )

par(mfrow = c(2, 1))
wdAvgSteps <- tapply( wd$steps, wd$interval, mean )
weAvgSteps <- tapply( we$steps, we$interval, mean )
newDay <- factor(rep( 0:1, each = 288 ), label = c( "weekday", "weekend" ) )
AvgSteps <- c( wdAvgSteps, weAvgSteps )
Intervals <- as.numeric( unlist( attributes( weAvgSteps )[2]) )
Intervals <- c(Intervals,Intervals)


library(lattice)
xyplot(AvgSteps  ~ Intervals  | newDay, layout = c(1, 2), xlab = "Interval" , ylab = "Number of steps", panel = function(x, y, ...) {
panel.xyplot(x, y, type = "l", ...)
})





