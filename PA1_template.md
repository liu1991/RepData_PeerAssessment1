---
output: html_document
---
#**Assessment1 of Reproducible Research**

This is the assessment1 of Reproducible Research, arming to analysis data from a personal activity monitoring device. More detailed information can be found from <https://github.com/rdpeng/RepData_PeerAssessment1>


###**Load and preprocess the data**


```r
act <- read.csv("activity.csv") # set working directory into the same folder with the file
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


###**What is mean total number of steps taken per day?**

To answer the question, it is needed to: <br/>
*1. Calculate the total number of steps taken per day* <br/>
*2. Make a histogram of the total number of steps taken each day* <br/>
*3. Calculate and report the mean and median of the total number of steps taken per day* <br/>


```r
s <- tapply(act$steps,act$date,sum)
hist(s,xlab="Total number of steps",main="Histogram of total number of steps each day")
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

###**What is the average daily activity pattern?**

To answer the question, it is needed to: *Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days* 


```r
m <- tapply(act$steps,act$interval,mean,na.rm=T)
plot(unique(act$interval),m,type="l",xlab="5-minute interval",ylab="Average number of steps",main="Time series of average number of steps across all day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
names(which(m==max(m)))
```

```
## [1] "835"
```

It can be seen that the 835th 5-minute interval contains the maximum number of steps across the day.

###**Imputing missing values**

*1. Calculate and report the total number of missing values in the dataset.* <br/>
*2. Use the MEAN of 5-minute interval to fill in all of the missing values in the dataset.* <br/>
*3. Create a new dataset that is equal to the original dataset but with the missing data filled in.*<br/>
*4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.*


```r
nrow(act)-sum(complete.cases(act)) # the total number of rows containing missing values
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
hist(s1,xlab="Total number of steps",main="Histogram of total number of steps each day")
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

It can be seen that the mean value is exactly the same just because the mean-based filling strategy. The median value increase slightly by imputing missing data. The impact of imputing missing data on the estimates of the total daily number of steps can be illustrated as below:


```r
plot(density(s1,na.rm=T),lty=1,col="red",main="Density of total daily number of steps from original and filled data")
lines(density(s,na.rm=T),lty=2,col="blue")
legend("topleft",c("Filled missing value","Original data"),lty=c(1,2),col=c("red","blue"))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 


###**Are there differences in activity patterns between weekdays and weekends?**

To answer the question, it is needed to: <br/>
*1. Create a new factor variable in the dataset with two levels ¨C ¡°weekday¡± and ¡°weekend¡± indicating whether a given date is a weekday or weekend day.* <br/>
*2. Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.* <br/>


```r
Sys.setlocale("LC_TIME", "English") #set the local time
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
xyplot(mean~interval|week,newact2,type="l",xlab="5-minute interval",ylab="Average number of steps",main="Time series of average number of steps across weekday and weekend")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

