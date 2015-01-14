library(ggplot2)

## Reading and preprocessing the data
setwd("./")
activity <- read.csv(unz("activity.zip", "activity.csv"))
# make copy
activityCopy <- activity
# Convert date to date class
activity$Date <- as.Date(activity$date, "%Y-%m-%d")
# Convert interval to a factor
activity$interval <- as.factor(activity$interval)
# Extract levels of 5-min intervals
l <- levels(activity$interval)
# view first 6 observations
head(activity)

## Average total number of steps taken per day
# Use tapply function to find the total, mean and median number of steps
# each day
totalSteps <- tapply(activity$steps, activity$date, sum, na.rm = T)
avgSteps <- tapply(activity$steps, activity$date, mean, na.rm = T)
# Make a histogram of the total number of steps taken each day
par(mfrow = c(1, 2))
hist(totalSteps, breaks = 20, col = "red", main = "Distribution of the total Number of steps daily", 
     xlab = "Total Number of Steps Daily")
hist(as.vector(avgSteps), breaks = 20, col = "blue", main = "Distribution of the Average Number of steps daily", 
     xlab = "Average Number of Steps Daily")
#Calculate and report the mean and median total number of steps taken per day
totalDailySteps <- as.numeric(tapply(activity$steps, activity$date, sum))
meanDailySteps <- mean(totalDailySteps, na.rm = T)
medianDailySteps <- median(totalDailySteps, na.rm = T)
print(meanDailySteps)
print(medianDailySteps)


## Average daily activity pattern
# Find the average number of steps grouped by intereval
#Steps = tapply(activity$steps, activity$interval, mean, na.rm = T)
# Convert levels of intervals into numeric
#Interval <- as.numeric(l)
# Create the dataframe df of the Interval and Steps columns
#df <- data.frame(Steps, Interval)

#library(ggplot2)

#g <- ggplot(df, aes(Interval, Steps))
#g + geom_line(colour = "blue") + ggtitle("Time Series Plot of the 5-minute Interval\n and the Average Number of Steps,\n Taken across all Days") + 
#        ylab("Average Number of Steps")

## data$interval <- as.factor(as.character(data$interval))
meanInterval <- as.numeric(tapply(activity$steps, activity$interval, mean, na.rm = T))
intervals <- data.frame(intervals = as.numeric(levels(activity$interval)), meanInterval)
intervals <- intervals[order(intervals$intervals), ]

par(mfrow = c(1, 1))

labels <- c("00:00", "05:00", "10:00", "15:00", "20:00")
labels.at <- seq(0, 2000, 500)
plot(intervals$intervals, intervals$meanInterval, type = "l", main = "Average steps 5-minute interval", 
     ylab = "Average steps", xlab = "Time of day", xaxt = "n")
axis(side = 1, at = labels.at, labels = labels)


sortedIntervals <- intervals[order(intervals$meanInterval, decreasing = T),]
head(sortedIntervals)
## Which 5-minute interval, on average across all the days in the dataset, 
## contains the maximum number of steps?
maxInterval <- sortedIntervals$intervals[1[1]]
print(maxInterval)
## The 5-minute interval with the highest average number of steps corresponds 
## to the interval between 8:35 AM and 8:40 AM.

## Imputing missing values
dim(activity[is.na(activity$steps), ])[1]

## The total number of missing values in the dataset (i.e. the total number of
## rows with NAs) is 2304.

## The strategy for filling in all of the missing values in the dataset is to 
## change the "NA"s to the mean values for that 5-minute interval.

steps <- vector()
for (i in 1:dim(activity)[1]) {
        if (is.na(activity$steps[i])) {
                steps <- c(steps, intervals$meanInterval[intervals$intervals == activity$interval[i]])
        } else {
                steps <- c(steps, activity$steps[i])
        }
}

## Create a new dataset that is equal to the original dataset but with the 
## missing data filled in.

activityWithoutNA <- data.frame(steps = steps, date = activity$date, 
                                            interval = activity$interval)
## Make a histogram of the total number of steps taken each day and 


par(mfrow = c(1, 1))
hist(tapply(activityWithoutNA$steps, activityWithoutNA$date, 
            sum), xlab = "Total daily steps", breaks = 20, 
     main = "Total of steps taken per day")

## Calculate and report the mean and median total number of steps taken per day. 

totalDailySteps <- as.numeric(tapply(activityWithoutNA$steps, 
                                       activityWithoutNA$date, sum))
meanTotalSteps <- mean(totalDailySteps)
medianTotalSteps <- median(totalDailySteps)
print(meanTotalSteps)
print(medianTotalSteps)

## Do these values differ from the estimates from the first part of the assignment? 

## The new mean and median of total number of steps taken per day are
## 10766 and 10766 respectively, the median is exactly equal to the mean. 

## What is the impact of imputing missing data on the estimates of the total 
## daily number of steps?

## Because of the strategy chosen, there is no impact of imputing missing 
## data on the estimates of the total daily number of steps.


## Are there differences in activity patterns between weekdays and weekends?

## Create a new factor variable in the dataset with two levels 
## - "weekday" and "weekend" indicating whether a given date is a weekday or 
## weekend day.

activityWithoutNA$day.type <- c("weekend", "weekday", "weekday", 
                                            "weekday", "weekday", "weekday", "weekend")[as.POSIXlt(activityWithoutNA$date)$wday + 
                                                                                                1]
activityWithoutNA$day.type <- as.factor(activityWithoutNA$day.type)

weekday <- activityWithoutNA[activityWithoutNA$day.type == 
                                                 "weekday", ]
weekend <- activityWithoutNA[activityWithoutNA$day.type == 
                                                 "weekend", ]

meansWeekday <- as.numeric(tapply(weekday$steps, weekday$interval, mean))
meansWeekend <- as.numeric(tapply(weekend$steps, weekend$interval, mean))

dayTypeIntervals <- data.frame(intervals = as.numeric(levels(activity$interval)), 
                                 meansWeekday, meansWeekend)
dayTypeIntervals <- dayTypeIntervals[order(dayTypeIntervals$intervals),]

## Make a panel plot containing a time series plot (i.e. type = "l") of the 
## 5-minute interval (x-axis) and the average number of steps taken, 
## averaged across all weekday days or weekend days (y-axis).

par <- par(mfrow = c(1, 2))
plot(dayTypeIntervals$intervals, dayTypeIntervals$weekday.means, type = "l", 
     col = "red", ylab = "Average steps", xlab = "Time of day", main = "Average steps 5-minute interval at weekday", 
     xaxt = "n")
axis(side = 1, at = labels.at, labels = labels)
plot(dayTypeIntervals$intervals, dayTypeIntervals$weekend.means, type = "l", 
     col = "blue", ylab = "Average steps", xlab = "Time of day", main = "Average steps 5-minute interval at weekend", 
     xaxt = "n")
axis(side = 1, at = labels.at, labels = labels)
