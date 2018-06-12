---
title: "Reproducible Research: Peer Assessment 1"
author: "Scott Fan"
date: "12/06/2018"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


To process the data, we first set the default for all code for echo to be True.


```r
knitr::opts_chunk$set(echo = TRUE)
```

We then load in the plotting library ggplot2, data.table and dplyr that will be used subsequently.


```r
library(ggplot2)
library(dplyr)
library(data.table)
```

Data is stored onto the desktop, therefore we will set that as working directory. We then load in the data. Since the date column of the data is not a datetime object, we will convert it to a datetime object for further processing. 


```r
setwd("~/Desktop")
rawdata <- read.csv("activity.csv")
rawdata$date <- as.Date(rawdata$date)  
```





## What is mean total number of steps taken per day?

Firstly, convert data to data.table, then sum by the date. The Histogram below shows the total number of steps taken per day.

```r
TD<- data.table(rawdata)
TD2 <- TD[,sum(steps), by = date]
colnames(TD2) <- c("Date", "Total Steps Per Day")
TD2 <- na.omit(TD2)
hist(TD2$`Total Steps Per Day`, main = " Histogram of Total Steps Per Day", xlab="Total Steps Per Day", breaks = 50)
```

![](Repdata_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

To get the mean and median of the Total Steps Per Day, we will use the mean and median functions. Numbers will be rounded to a whole number for it to be more meaningful. 

```r
mean(TD2$`Total Steps Per Day`)
```

```
## [1] 10766.19
```


```r
median(TD2$`Total Steps Per Day`)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

First, convert intervals to factor, and then aggregate it. We can then plot a time series graph to show activity across time. 


```r
TD3 <- na.omit(TD)
TD3$interval <- as.factor(TD3$interval)
TD3 <- aggregate(TD3$step~TD3$interval, FUN=mean)
names(TD3) <- c("Time Interval", "Steps")
```


```r
plot(x = TD3$`Time Interval`, y = TD3$Steps, main = "Number of Steps over 5 Minute Intervals Per Day (On Average)", xlab = "Time Intervals", ylab = " Steps ")
lines(x = TD3$`Time Interval`, y = TD3$Steps)
```

![](Repdata_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

To determine which time period has the highest activity. 

```r
TD3$`Time Interval`[TD3$Steps==max(TD3$Steps)]
```

```
## [1] 835
## 288 Levels: 0 5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 ... 2355
```

## Imputing missing values

To siphon out the number of NA values, 

```r
sum(is.na(TD))
```

```
## [1] 2304
```

To populate the missing values, I chose to replace the values with the mean of the interval at that pooint of the day, namely from TD3, with a function called NA_fill.


```r
NA_fill <- function(rawdata, filler) {
    NA_index <- which(is.na(rawdata$steps))
    NA_replace <- unlist(lapply(NA_index, FUN= function(idx){
        interval = rawdata[idx,]$interval
        filler[filler$'Time Interval'== interval,]$Steps
    }))
    fill_steps <- rawdata$steps
    fill_steps[NA_index] <- NA_replace
    fill_steps
}

filled_data <- data.frame(Steps = NA_fill(rawdata, TD3), date = rawdata$date, interval =rawdata$interval)
```

Histogram of the total number of steps taken each day.

```r
TD4<- data.table(filled_data)
TD4 <- TD4[,sum(Steps), by = date]
colnames(TD4) <- c("Date", "Total Steps Per Day")
hist(TD4$`Total Steps Per Day`, main = " Histogram of Total Steps Per Day", xlab="Total Steps Per Day", breaks = 50)
```

![](Repdata_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Mean of total steps taken each day.

```r
mean(TD4$`Total Steps Per Day`)
```

```
## [1] 10766.19
```

Median of total steps taken each day.

```r
median(TD4$`Total Steps Per Day`)
```

```
## [1] 10766.19
```

From the results, we see that there has been a significant shift in the Median, which takes on the same value as the Mean. 


The shape of the graph has also changed to reflect the effects of filling in the empty data. 

## Are there differences in activity patterns between weekdays and weekends?

We subset out the days which are in the weekends.


```r
filled_data$DoTW <- weekdays(filled_data$date)

weekend_data <- subset(filled_data, DoTW %in% c("Saturday","Sunday"))
weekend_data$interval <- as.factor(weekend_data$interval)
weekend_data2 <- aggregate(weekend_data$Steps~weekend_data$interval, FUN=mean)
names(weekend_data2) <- c("Time Interval", "Steps")
```

We the subset out the days in the weekdays.

```r
weekday_data <- subset(filled_data, !DoTW %in% c("Saturday","Sunday"))
weekday_data$interval <- as.factor(weekday_data$interval)
weekday_data2 <- aggregate(weekday_data$Steps~weekday_data$interval, FUN=mean)
names(weekday_data2) <- c("Time Interval", "Steps")
```


```r
par(mfrow = c(2, 1))
plot(x = weekend_data2$`Time Interval`, y = weekend_data2$Steps, main = "Number of Steps over 5 Minute Intervals Per Day (On Average Weekend)", xlab = "Time Intervals", ylab = " Steps ")
lines(x = weekend_data2$`Time Interval`, y = weekend_data2$Steps)

plot(x = weekday_data2$`Time Interval`, y = weekday_data2$Steps, main = "Number of Steps over 5 Minute Intervals Per Day (On Average Weekday)", xlab = "Time Intervals", ylab = " Steps ")
lines(x = weekday_data2$`Time Interval`, y = weekday_data2$Steps)
```

![](Repdata_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

We therefore see that the peak in weekdays are definitely higher. However, there are more peaks in the weekends. This could be due to the sedentary office lifestyle that most this subject has. 



