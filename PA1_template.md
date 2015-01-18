# Reproducible Research: Peer Assessment 1



## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web site: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Loading and preprocessing the data

```r
# Read the data from file
setwd("./")  
activity_data = read.csv(unz("activity.zip", "activity.csv"),stringsAsFactors = F)
# format the data and transform NA to 0
activity_data$date <- as.Date(activity_data$date, "%Y-%m-%d")
#activity_data$steps[is.na(activity_data$steps)] = 0
head(activity_data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(activity_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?

```r
# Organize the date by days, ignoring the missing values
# Calculate and report the mean and median total number of steps taken per day
sum_steps_per_day <- aggregate(steps ~ date, data = activity_data, sum)
mean_steps = as.integer(mean(sum_steps_per_day$step, na.rm = T))
median_steps = as.integer(median(sum_steps_per_day$step, na.rm = T))
```
- The mean total number of steps taken per day is 10766.00.  
- The median total number of steps taken per day is 10765.00.


```r
# Plot histogram of the total number of steps taken each day
hist(sum_steps_per_day$steps, 
     main="Total Number of Steps Taken Each Day",
     breaks = 20,
     col = "red",
     xlab = "total number of steps taken per day"
     ) 
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

## What is the average daily activity pattern?

```r
# Organize the data by daily time series
daily_steps <- aggregate(steps ~ interval, data = activity_data, mean)
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
plot(daily_steps$interval, 
     daily_steps$steps,
     type = "l",
     main = "Average Number of Steps Taken Each 5-Minutes Interval",
     xlab = "time interval",
     ylab = "average steps taken")

abline(v = daily_steps[daily_steps$steps == max(daily_steps$steps), 1], 
       col = "red")
text(x = daily_steps[daily_steps$steps == max(daily_steps$steps),1],
     y = max(daily_steps$steps),
     labels= max(daily_steps$steps),
     cex= 0.7, offset = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

Which 5-minute interval (on average across all the days in the dataset) contains the maximum number of steps?

```r
maximun_activity_pattern  = daily_steps[daily_steps$steps == max(daily_steps$steps),1]
```
The 835th 5-minute interval contains the maximum number of steps.

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.


```r
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
NA_num = sum(is.na(activity_data$steps))
print(NA_num)
```

```
## [1] 2304
```
The total number of missing values in the dataset is 2304.

The strategy selected to fill-in all of the missing values in the dataset is to replace with the mean for that 5-minute interval.

```r
# Create a new dataset that is equal to the original dataset but with the missing data filled-in
new_activity_data <- activity_data
for (i in 1:length(new_activity_data$steps)) {
       if (is.na(new_activity_data$steps[i])) {
               mean_activity <- daily_steps[daily_steps$interval == new_activity_data$interval[i], 2]
               new_activity_data$steps[i] <- mean_activity }        
}

# Make a histogram of the total number of steps taken each day
new_sum_steps_per_day <- aggregate(steps ~ date, data = new_activity_data, sum)
new_mean_steps = as.integer(mean(new_sum_steps_per_day$step, na.rm = T))
new_median_steps = as.integer(median(new_sum_steps_per_day$step, na.rm = T))

# Plot the histogram of the total number of steps taken each day
hist(new_sum_steps_per_day$steps, 
     main="Total Number of Steps Taken Each Day with Imputing Missing Values",
     col = "blue",
     breaks = 20,
     xlab = "total number of steps taken per day"
     ) 
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 


```r
# Calculate and report the mean and median total number of steps taken per day
new.total.daily.steps <- as.numeric(tapply(new_activity_data$steps, 
                                       new_activity_data$date, sum))
new.step.mean <- mean(new.total.daily.steps)
new.step.median <- median(new.total.daily.steps)
print(new.step.mean)
```

```
## [1] 10766.19
```

```r
print(new.step.median)
```

```
## [1] 10766.19
```

After replacing 2304.00 missing values with mean value of that interval, the mean total number of steps taken per day was 10766.19, and the median total number of steps taken per day was 10766.19. 
It is observed that by imputing missing values, the mean value remains the same while the median increased by a little.

### What is the impact of imputing missing data on the estimates of the total daily number of steps?

By comparing with the calculations done in the first section of this document, it is observed that while the mean value remains unchanged, the median value has shifted and virtual matches to the mean.

Since our data has shown a t-student distribution (see both histograms), it seems that the impact of imputing missing values has increase our peak, but it does not affect our predictions negatively. 

## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function and the dataset with the filled-in missing values is used.

The appraoch is to split the data by week_label and time interval, then calculate the mean steps for each combine of weeklabel and time interval, using lattice to give a plot to visually show the answer.



```r
# Using the dataset with the filled-in missing values, create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
weekdays = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekends = c("Saturday", "Sunday")

# add week labels to the dataset
week_label = vector()
new_activity_data$date <- weekdays(new_activity_data$date)
for (i in 1:length(new_activity_data$date)){
        if (new_activity_data$date[i] %in% weekdays){
                week_label = c(week_label, "weekday")  }
        else{
                week_label = c(week_label, "weekend")  }
}
new_activity_data <- transform( new_activity_data, week_labels = week_label)

# split the data by week label and interval then calcualte the mean steps, and plot
mean_steps_per_interval <- aggregate(steps ~ interval + week_labels, 
                                     data = new_activity_data, mean)
library(lattice)
xyplot(steps ~ interval|week_labels, 
       data = mean_steps_per_interval,
       type = "l",
       xlab = "Interval",
       ylab = "Number of steps",
       layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

- It is observed that people get more active on weekends, probably due to them having more avaliable time on weekend.
- It is observed that at the graph above that activity on the weekday has the greatest peak from all steps intervals. 
- But we can observe too that weekends activities has more peaks over a hundred than weekday. This could be due to the fact that activities on weekdays mostly follow a work related routine, where we find some more intensity activity in little a free time that the employ can made some sport. 
- On the other hand, at weekend we can see better distribution of effort along the time.
