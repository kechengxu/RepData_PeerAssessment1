# Report of Daily Acitivity
## ---based on data from a personal activity montoring device

### Loading and Processing the data
* First, let's load the data.

```r
setwd("~/Desktop/Coursera/5-reproducible /project 1")
data<-read.csv("activity.csv")
```

### What is mean total number of steps taken per day?
* Below is a histogram of the total number of steps taken per day:

```r
s<-split(data,data$date)
total<-sapply(s,function(x) sum(x[,c("steps")],na.rm=TRUE))
hist(total,main="Histogram of Total Number of Steps per day",xlab="Number of Steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
average<-mean(total)
middle<-median(total)
```

* The mean of the total number of steps taken per day is 9354.2295, and the median is 10395.

### What's the daily average activity pattern?
* Below is a time series plot of the 5-minute interval (x-axis) and the average number of steps taken averaged across all days (y-axis):

```r
m<-split(data,data$interval)
interval<-sapply(m,function(x) unique(x[,c("interval")]))
interval_mean<-sapply(m,function(x) mean(x[,c("steps")],na.rm=TRUE))
plot(interval,interval_mean,type="l",xlab="time interval",ylab="average number of steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
interval_mean_max<-max(interval_mean)
max_interval<-names(interval_mean)[which(interval_mean==interval_mean_max)]
```

* On the time interval of 835, it contains the maximum number of steps averaged across all days.

### Imputting missing values

```r
Number_NA<-sum(is.na(data$steps))
```
* In the above analysis, there are 2304 missing values. The presence of missing values may introduce bias into our analysis. 

* Now, let's fill in all the missing values with the mean for the 5-minute interval averaged across all days, and reanalyze the data. 

```r
for (i in 1:length(data$steps)) {
    if (is.na(data$steps[i])==TRUE) {
       data$steps[i]=interval_mean[as.character(data$interval[i])] 
    } 
}
```

* Below is a new histogram of the total number of steps taken per day:

```r
n<-split(data,data$date)
new_total<-sapply(n,function(x) sum(x[,c("steps")],na.rm=TRUE))
hist(new_total,main="Histogram of New Total Number of Steps per day",xlab="Number of Steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

```r
new_average<-mean(new_total)
new_middle<-median(new_total)
```

* Now, the mean of the toal number of steps taken per day is 1.0766 &times; 10<sup>4</sup>, and the median is 1.0766 &times; 10<sup>4</sup>. Both of them are quite different from previous results. Compare the new mean and median with previous values, we find that by imputting the missing values, both the mean and median are enlarged.

### Are there differences in activity patterns between weekdays and weekends?
* Below is a panel plot comparing the activity patterns between weekdays and weekends, where x-axis represents the 5-minute time interval and y-axis represents the average number of steps taken averaged across all weekday days or all weekend days.

```r
# create a new factor variable and add it into the dataset
f<-factor(c("weekday","weekend"))
data<-data.frame(data,f)

# transform the Date column into time format and fill in the new variable
date<-as.Date(data$date,"%Y-%m-%d")
for (i in 1:length(date)) {
    if (weekdays(date[i]) %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")) {
        data$f[i]="weekday"
    }
    else {
        data$f[i]="weekend"
    }
}

# divide the dataset into two dataframes according to the levels of the new variable
weekday_data<-subset(data,data$f=="weekday")
p<-split(weekday_data,weekday_data$interval)
weekday_mean<-sapply(p,function(x) mean(x[,c("steps")]))
weekend_data<-subset(data,data$f=="weekend")
q<-split(weekend_data,weekend_data$interval)
weekend_mean<-sapply(q,function(x) mean(x[,c("steps")]))

# merge the data into a new dataframe and make the plots
steps_mean<-c(weekend_mean,weekday_mean)
newinterval<-rep(interval,2)
ninterval<-length(interval)
f<-c(rep("weekend",ninterval),rep("weekday",ninterval))
newdata<-data.frame(steps_mean,newinterval,f)
library(lattice)
xyplot(steps_mean~newinterval|f,data=newdata,type="l",
       main="diffference in activity patterns between weekdays and weekends",
       xlab="time interval",ylab="average number of steps",layout=c(1,2))
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

* According to the above plot, we can see that the activity patterns between weekdays and weekends are quite different.Generally speaking, on weekdays the steps the person take are peaked around 8:00 am-10:00 am, while at weekends the steps are relatively evenly distributed across 8:00 am-8:00 pm.
