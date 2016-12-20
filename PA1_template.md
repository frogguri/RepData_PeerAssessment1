# Reproducible Research - Course Project 1




### Loading and preprocessing the data

```r
Act <- read.csv("activity.csv")
```

### What is mean total number of steps taken per day?  
1. Calculate the total number of steps taken per day

```r
ActStep <- tapply(Act$steps, Act$date, sum, na.rm=TRUE)
ActStep
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015          0      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414          0      10600      10571          0      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219          0          0      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336          0         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##          0
```

2. Make a histogram of the total number of steps taken each day

```r
hist(ActStep, breaks=20, xlab="Steps", main="Total number of steps taken per day")
```

![](PA1_template_files/figure-html/step per day hist-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(ActStep)
```

```
## [1] 9354.23
```

```r
median(ActStep)
```

```
## [1] 10395
```

### What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
ActPat <- aggregate(steps ~ interval, data=Act, mean, na.rm=TRUE)
plot(ActPat, type = "l", xlab="Interval (min)", ylab="Steps", main=" Average number of steps")
```

![](PA1_template_files/figure-html/daily pattern plot-1.png)<!-- -->

  2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
ActPat[which.max(ActPat$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

### Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(Act))
```

```
## [1] 2304
```

  2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  

- Replacing NA to mean of 5-minute interval

  3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
mergedAct <- merge(Act, ActPat, by="interval")
mergedAct$steps.x[is.na(mergedAct$steps.x)] = mergedAct$steps.y[which(is.na(mergedAct$steps.x))]
sum(is.na(mergedAct))
```

```
## [1] 0
```

  4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
mergedActStep <- tapply(mergedAct$steps.x, mergedAct$date, sum)
hist(mergedActStep, breaks=20, xlab="Steps", main="Total number of steps taken per day")
```

![](PA1_template_files/figure-html/imputing NA hist-1.png)<!-- -->


```r
mean(mergedActStep)
```

```
## [1] 10766.19
```

```r
median(mergedActStep)
```

```
## [1] 10766.19
```

  - Both histograms have similar pattern except decreased 0 values in the dataset imputing NA compared to the original dataset in which NA set as 0 by default. Mean of total number of steps per day increased by replacing those 0s to each mean of 5-minute interval.
  
  
### Are there differences in activity patterns between weekdays and weekends?
  1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
mergedAct$date <- as.Date(mergedAct$date, "%Y-%m-%d")
mergedAct$wkday <- weekdays(mergedAct$date)
wd <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
mergedAct$wkday <- factor(mergedAct$wkday %in% wd, levels=c(FALSE, TRUE), labels=c("weekend", "weekday"))
str(mergedAct$wkday)
```

```
##  Factor w/ 2 levels "weekend","weekday": 2 2 1 2 1 2 1 2 2 1 ...
```

  2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
stepWd <- aggregate(steps.x ~ interval+wkday, mergedAct, mean)
library(lattice)
xyplot(steps.x ~ interval | wkday, data=stepWd, layout=c(1, 2), type="l", ylab="Steps", xlab="Interval (min)")
```

![](PA1_template_files/figure-html/weekday plot-1.png)<!-- -->
