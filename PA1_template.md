---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

 
## Loading and preprocessing the data

Load the data


```r
activity_data <- read.csv(unz("activity.zip", "activity.csv"), header = TRUE, sep = ",")
```

Process/transform the data (if necessary) into a format suitable for your analysis


```r
activity_data$date <- as.Date(activity_data$date)
```


## What is mean total number of steps taken per day?

Calculate the total number of steps taken per day.
If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.


```r
library(ggplot2)
steps_day <- aggregate(steps ~ date, data=activity_data, FUN=sum,na.rm = TRUE)
ggplot(steps_day, aes(x=steps)) +
    geom_histogram(binwidth=2500, colour="black", fill="white")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Calculate and report the mean and median total number of steps taken per day


```r
mean(steps_day$steps)
```

```
## [1] 10766.19
```

```r
median(steps_day$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps_interval <- aggregate(steps ~ interval, data=activity_data,
                               FUN=mean, na.rm = TRUE)
ggplot(steps_interval, aes(x=interval, y=steps)) +
  geom_line(stat="identity")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
steps_interval[which(steps_interval$steps == max(steps_interval$steps)), ]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(!complete.cases(activity_data))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The strategy will be to fill The missing values with the mean for an interval.

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity_data_filled<-activity_data
activity_data_filled$steps[is.na(activity_data_filled$steps)] <- mean(na.omit(activity_data$steps))
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
steps_day_1 <- aggregate(steps ~ date, data=activity_data_filled, FUN=sum)
ggplot(steps_day_1, aes(x=steps)) +
    geom_histogram(binwidth=2500, colour="black", fill="white")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

```r
mean(steps_day_1$steps)
```

```
## [1] 10766.19
```

```r
median(steps_day_1$steps)
```

```
## [1] 10766.19
```

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

The values are only slightly higher. The impact of imputing missing data with this strategy is very low. The mean is the same because the missing values have been replaced with the mean for an interval.

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day


```r
wday<-weekdays(activity_data$date)
wday[wday=="lunes"|wday=="martes"|wday=="miércoles"|wday=="jueves"|wday=="viernes"]<-"weekday"
wday[wday=="sábado"|wday=="domingo"]<-"weekend"
#create factor
wday<-factor(wday)
#add fafctor to data frame
activity_data<-cbind(wday, activity_data)
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
steps_day_new <- aggregate(steps ~ interval + wday, data=activity_data, FUN=mean)
ggplot(steps_day_new, aes(x=interval, y=steps, group=1)) + geom_line() +
    facet_wrap(~ wday, ncol=1)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

We can conclude that there are differences in activity patterns between weekdays and weekends. During weekdays the activity is more concntrated in the morning. Also, the activity starts and ends earlier during weekdays.

