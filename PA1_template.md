---
title: "PA1_template"
output: html_document
---

This is an R Markdown for assignment1 of Reproducible Research. More information can be found <https://github.com/rdpeng/RepData_PeerAssessment1>


####Loading and preprocessing the data
1. Load the data 
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
act <- read.csv("activity.csv")
head(act)
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


####What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day


```r
s <- tapply(act$steps,act$date,sum)
hist(s)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
mean(s,na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(s,na.rm=TRUE)
```

```
## [1] 10765
```

####What is the average daily activity pattern?
1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
m <- tapply(act$steps,act$interval,mean,na.rm=T)
plot(unique(act$interval),m,type="l")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
names(which(m==max(m)))
```

```
## [1] "835"
```

####Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
nrow(act)-sum(complete.cases(act))
```

```
## [1] 2304
```

```r
missind <- which(is.na(act$steps)==TRUE)
newact <- act
for (i in missind) {
  newact$steps[i] <- mean(newact$steps[newact$interval==newact$interval[i]],na.rm=TRUE)
}

s1 <- tapply(newact$steps,newact$date,sum)
hist(s1)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
mean(s1)
```

```
## [1] 10766.19
```

```r
median(s1)
```

```
## [1] 10766.19
```

```r
mean(s1)-mean(s,na.rm=TRUE)
```

```
## [1] 0
```

```r
median(s1)-median(s,na.rm=TRUE)
```

```
## [1] 1.188679
```

####Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels ¨C ¡°weekday¡± and ¡°weekend¡± indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
weekd <- weekdays(as.Date(act$date))
week <- ifelse(weekd=="Saturday","weekend",ifelse(weekd=="Sunday","weekend","weekday"))
newact1 <- data.frame(newact,week=week)


library(lattice)
sub1 <- subset(newact1,newact1$week=="weekday")
sub2 <- subset(newact1,newact1$week=="weekend")
mean1 <- tapply(sub1$steps,sub1$interval,mean)
mean2 <- tapply(sub2$steps,sub2$interval,mean)
newact2 <- data.frame(mean=c(mean1,mean2),interval=rep(unique(act$interval),times=2),week=rep(c("weekday","weekend"),each=length(mean1)))
xyplot(mean~interval|week,newact2,type="l",xlab="steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

