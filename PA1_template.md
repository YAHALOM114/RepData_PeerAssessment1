---
title: "Peer Assessment Assignment 1"
output: html_document
---
###Introduction
This assignment makes use of data from a personal activity monitoring device. 
This device collects data at 5 minute intervals throughout the day. 
The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day

###Data

The dataset for this assignment was downloaded from the course web site <https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip>

The dataset includes 3 variables:

   • steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
 
   • date: The date on which the measurement was taken in YYYY-MM-DD format
 
   • interval: Identifier for the 5-minute interval in which measurement was taken
   
###Initialization

####Adjusting System Time

```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```
####Loading *Measured Data* from the dataset

```r
full_data<- read.csv("activity.csv")
```
####loading required R packages

```r
library(dplyr)
library(lattice)
```
###Processing
####Keeping original data unprocessed

```r
d<-full_data
```
####Transforming data to suitable format

```r
d$date<- as.Date(d$date, "%Y-%m-%d")
d1<- aggregate(d$steps,by=list(d$date),FUN=sum)
```
###Histogram of the total number of steps taken each day

```r
hist(d1$x, xlab= "steps per day (bins)", ylab="number of days (frequency)", main="Histogram of steps per day")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

####MEAN and MEDIAN of the total number of steps taken per day. MEAN=10766.19 and MEDIAN=10765

```r
mean(d1[,2], na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(d1[,2], na.rm=TRUE)
```

```
## [1] 10765
```

####Time series plot of the average number of steps taken in every interval across all days 

```r
d<- na.omit(d)
d2<- aggregate(d$steps, by=list(d$interval), FUN=mean)
plot(d2[,1], d2[,2], type="l", xlab="Interval", ylab="Average number of steps (per interval")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

####The Interval that contains, on average, the largest number of steps is Interval 835


```r
filter(d2,x==max(d2[,2]))[1]
```

```
##   Group.1
## 1     835
```

###Imputing missing values
####Total number of missing values = 2304

```r
sum(is.na(full_data))
```

```
## [1] 2304
```
####Creating new data set by replacing the NAs with the mean of all interval values that are not equal to NA

```r
dnna<-replace(full_data, is.na(full_data), mean(full_data[,1], na.rm = TRUE))
```
###Histogram,based on the new data set, of the total number of steps taken each day

```r
d1nna<- aggregate(dnna$steps,by=list(dnna$date),FUN=sum)
hist(d1nna$x, xlab= "steps per day (bins)", ylab="number of days (frequency)", main="Histogram of steps per day")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

####Calculating the MEAN and MEDIAN based on the new data set. 

The new MEAN=10766.19 and is equal to the previous MEAN and to the new MEDIAN=10766.19 that is not equal to previous MEDIAN.

```r
mean(d1nna[,2])
```

```
## [1] 10766.19
```

```r
median(d1nna[,2])
```

```
## [1] 10766.19
```

####Compering Weekend activity pattern vs Weekday activity pattern

#####Creating factor of 2 levels:"weekday", "weekend"

```r
d2nna<- mutate(dnna, "weekpart"=(weekdays(as.POSIXlt(dnna[,2]))=="Sunday"|weekdays(as.POSIXlt(dnna[,2]))=="Saturday" ))
d2nnawd<-filter(d2nna,weekpart==FALSE)
d2nnawe<-filter(d2nna,weekpart==TRUE)
d3nnawd<- aggregate(d2nnawd$steps, by=list(d2nnawd$interval), FUN=mean)
d3nnawe<- aggregate(d2nnawe$steps, by=list(d2nnawe$interval), FUN=mean)
d3nna<-rbind(mutate(d3nnawe,weekpart="weekend"),mutate(d3nnawd,weekpart="weekday"))
```

####Plot of both activity patterns

```r
xyplot(d3nna[,2] ~ d3nna[,1]|d3nna$weekpart,d3nna, type="l", layout=c(1,2),xlab="interval", ylab="Average number of steps (per interval)")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) 
