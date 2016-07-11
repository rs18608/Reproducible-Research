# PA1_Activity_Monitoring.Rmd
Lynn Scott  
July 11, 2016  


## Load and pre-process data.


```r
setwd("/Users/rs186089/coursera/repro-research/RepData_PeerAssessment1")
unzip("activity.zip")
activity.data <- read.table("activity.csv", header=TRUE, sep=",")

str(activity.data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is the mean total number of steps taken per day?


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
total.steps.per.day = aggregate(activity.data$steps, list(date = activity.data$date), sum, na.rm = TRUE)
colnames(total.steps.per.day)[2] = "total.steps"

head(total.steps.per.day)
```

```
##         date total.steps
## 1 2012-10-01           0
## 2 2012-10-02         126
## 3 2012-10-03       11352
## 4 2012-10-04       12116
## 5 2012-10-05       13294
## 6 2012-10-06       15420
```

Below is the distribution of the total number of steps taken per day (missing values are ignored):


```r
hist(total.steps.per.day$total.steps, 
     main = "Distribution of Total Number of Steps per Day", 
     xlab = "Total Number of Steps per Day", 
     ylab = "Frequency (Number of Days)", 
     breaks=20, col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

### Mean of steps taken per day:

```r
mean(total.steps.per.day$total.steps, na.rm=TRUE)
```

```
## [1] 9354.23
```
### Median of steps taken per day:

```r
median(total.steps.per.day$total.steps, na.rm=TRUE)
```

```
## [1] 10395
```


## What is the average daily activity pattern?

Calcuate the average number of steps per 5-minute interval over the 2 month period:


```r
avg.steps.per.interval = aggregate(activity.data$steps, list(interval = activity.data$interval), mean, na.rm = TRUE)

colnames(avg.steps.per.interval)[2] = "avg.steps"

head(avg.steps.per.interval)
```

```
##   interval avg.steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```
The following graph represents the average daily activity pattern. It is a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):


```r
plot(strptime(sprintf("%04d", avg.steps.per.interval$interval), format="%H%M"),
     avg.steps.per.interval$avg.steps, type = "l", 
     main = "Average Daily Activity", 
     xlab = "Time of Day (HH:MM)", 
     ylab = "Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

By deriving the maximum of average steps from avg.setsp.per.interval, the interval the containst the maximum number of steps is show:


```r
  max.interval = filter(avg.steps.per.interval, avg.steps==max(avg.steps))
```

So the 5 minute interval that contains the maximum number of steps is: 835.

## Inputing missing values:

Total number of missing values:


```r
  sum(is.na(activity.data))
```

```
## [1] 2304
```

New data set with missing values is replaced with the mean for each interval of the original data set.


```r
activity.data.revised = inner_join(activity.data, avg.steps.per.interval, by="interval") %>%
                    mutate(steps=ifelse(is.na(steps), avg.steps, steps)) %>%
                    select(date, interval, steps)

head(activity.data.revised)
```

```
##         date interval     steps
## 1 2012-10-01        0 1.7169811
## 2 2012-10-01        5 0.3396226
## 3 2012-10-01       10 0.1320755
## 4 2012-10-01       15 0.1509434
## 5 2012-10-01       20 0.0754717
## 6 2012-10-01       25 2.0943396
```

Below is the distribution of the total number of steps taken per day (NAs have been normalized to contain the mean of the interval):


```r
total.steps.per.day = aggregate(activity.data.revised$steps, list(date = activity.data.revised$date), sum, na.rm = TRUE)
colnames(total.steps.per.day)[2] = "total.steps"

hist(total.steps.per.day$total.steps, 
     main = "Distribution of Total Number of Steps per Day (revised)", 
     xlab = "Total Number of Steps per Day", 
     ylab = "Frequency (Number of Days)", 
     breaks=20, col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

With the data normalized to revise missing intervals the mean does not change:

```r
mean(total.steps.per.day$total.steps, na.rm=TRUE)
```

```
## [1] 10766.19
```
but the median has changed to equal the mean.

```r
median(total.steps.per.day$total.steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

### Are there differences in activity patterns between weekdays and weekends?

First make factors for the the days of the week (i.e. weekday/weekend).


```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
activity.data.revised$day.of.week = as.factor(ifelse(is.element(weekdays(as.Date(activity.data.revised$date)),weekdays), "Weekday", "Weekend"))
                                   
head(activity.data.revised)
```

```
##         date interval     steps day.of.week
## 1 2012-10-01        0 1.7169811     Weekday
## 2 2012-10-01        5 0.3396226     Weekday
## 3 2012-10-01       10 0.1320755     Weekday
## 4 2012-10-01       15 0.1509434     Weekday
## 5 2012-10-01       20 0.0754717     Weekday
## 6 2012-10-01       25 2.0943396     Weekday
```

Calculate the average number of steps per calandar day:

```r
activity.pattern = activity.data.revised %>%
                    group_by(day.of.week, interval) %>%
                    summarize(avg.steps=mean(steps))

head(activity.pattern)
```

```
## Source: local data frame [6 x 3]
## Groups: day.of.week [1]
## 
##   day.of.week interval  avg.steps
##        (fctr)    (int)      (dbl)
## 1     Weekday        0 2.25115304
## 2     Weekday        5 0.44528302
## 3     Weekday       10 0.17316562
## 4     Weekday       15 0.19790356
## 5     Weekday       20 0.09895178
## 6     Weekday       25 1.59035639
```


Plot the average daily activity.

```r
library(ggplot2)

ggplot(activity.pattern, aes(interval, avg.steps, group=1)) +
    geom_line() +
    facet_grid(day.of.week~., scale='free_y') +
    xlab('Time of Day') +
    ylab('Average Number of Steps') +
    ggtitle("Average Daily Activity\nWeekday vs. Weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

The following conclusions can be drawn from the plot above:

* People are active in the early morning weekeday then the weekeend days
* Weekday ctivity is quite intense around 8:30AM
* Weekend activity is more spread out during the day
