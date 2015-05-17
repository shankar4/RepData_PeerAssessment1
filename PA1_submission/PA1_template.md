---
title: "PA1_template"
author: "Ravi Shankar"
date: "Sunday, May 17, 2015"
output: html_document
---
### set up caching and knitr chunk dependency calculation
``` {r setoptions,tidy=F, cache=F, eval=T, echo=F, results = "hide"}opts_chunk$set(autodep=T)dep_auto()
```

### Run the following data prep steps. Assumed that the working directory has a folder with .csv data:

### Part 1: Loading and preprocessing the data


```r
dir<- "C:/Users/shankar/Desktop/Reproducible_Research/Data/PA1_submission"
setwd(dir)
dir <- getwd()
sub.dir <- "repdata_data_activity/activity.csv"
data.dir <- paste(dir, sub.dir, sep="/")
fit.steps <- read.csv(data.dir)
paste('checked at',date())
```

```
## [1] "checked at Sun May 17 16:13:08 2015"
```
### Part 2: What is the mean total number of steps taken per day?

```r
#Remove Nas
fit.steps.good <- na.omit(fit.steps)
summary (fit.steps.good$date)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        288        288        288        288        288 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##        288          0        288        288        288        288 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##        288        288        288        288        288        288 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##        288        288        288        288        288        288 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##        288        288        288        288        288        288 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##        288          0        288        288          0        288 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##        288        288        288          0          0        288 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##        288        288          0        288        288        288 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##        288        288        288        288        288        288 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##        288        288        288        288        288        288 
## 2012-11-30 
##          0
```

```r
date.factor <- factor(fit.steps.good$date)
# Assignment, Part 2 step 1: Find total number of steps taken per day
day.sum<-tapply(fit.steps.good$steps, date.factor, sum)
day.sum
```

```
## 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##        126      11352      12116      13294      15420      11015 
## 2012-10-09 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 
##      12811       9900      10304      17382      12426      15098 
## 2012-10-15 2012-10-16 2012-10-17 2012-10-18 2012-10-19 2012-10-20 
##      10139      15084      13452      10056      11829      10395 
## 2012-10-21 2012-10-22 2012-10-23 2012-10-24 2012-10-25 2012-10-26 
##       8821      13460       8918       8355       2492       6778 
## 2012-10-27 2012-10-28 2012-10-29 2012-10-30 2012-10-31 2012-11-02 
##      10119      11458       5018       9819      15414      10600 
## 2012-11-03 2012-11-05 2012-11-06 2012-11-07 2012-11-08 2012-11-11 
##      10571      10439       8334      12883       3219      12608 
## 2012-11-12 2012-11-13 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
##      10765       7336         41       5441      14339      15110 
## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 
##       8841       4472      12787      20427      21194      14478 
## 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      11834      11162      13646      10183       7047
```

```r
# Assignment, Part 2 step 2: Make a histogram of the total number of steps taken each day"
pdf("HistogramStepsNAIgnored.pdf")
hist(day.sum,breaks=100,col='red', xlab="Total Steps Per Day", main='Histogram')
dev.off()
```

```
## RStudioGD 
##         2
```
##### The histogram was saved to HistogramStepsNAIgnored.pdf


```r
# Assignment, Part 2 step 3: Calculate and report mean and median of the total number of stepts taken per day
day.mean <-tapply(fit.steps.good$steps, date.factor, mean)
day.mean
```

```
## 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##  0.4375000 39.4166667 42.0694444 46.1597222 53.5416667 38.2465278 
## 2012-10-09 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 
## 44.4826389 34.3750000 35.7777778 60.3541667 43.1458333 52.4236111 
## 2012-10-15 2012-10-16 2012-10-17 2012-10-18 2012-10-19 2012-10-20 
## 35.2048611 52.3750000 46.7083333 34.9166667 41.0729167 36.0937500 
## 2012-10-21 2012-10-22 2012-10-23 2012-10-24 2012-10-25 2012-10-26 
## 30.6284722 46.7361111 30.9652778 29.0104167  8.6527778 23.5347222 
## 2012-10-27 2012-10-28 2012-10-29 2012-10-30 2012-10-31 2012-11-02 
## 35.1354167 39.7847222 17.4236111 34.0937500 53.5208333 36.8055556 
## 2012-11-03 2012-11-05 2012-11-06 2012-11-07 2012-11-08 2012-11-11 
## 36.7048611 36.2465278 28.9375000 44.7326389 11.1770833 43.7777778 
## 2012-11-12 2012-11-13 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
## 37.3784722 25.4722222  0.1423611 18.8923611 49.7881944 52.4652778 
## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 
## 30.6979167 15.5277778 44.3993056 70.9270833 73.5902778 50.2708333 
## 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
## 41.0902778 38.7569444 47.3819444 35.3576389 24.4687500
```

```r
day.median <-tapply(fit.steps.good$steps, date.factor, median)
day.median
```

```
## 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##          0          0          0          0          0          0 
## 2012-10-09 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 
##          0          0          0          0          0          0 
## 2012-10-15 2012-10-16 2012-10-17 2012-10-18 2012-10-19 2012-10-20 
##          0          0          0          0          0          0 
## 2012-10-21 2012-10-22 2012-10-23 2012-10-24 2012-10-25 2012-10-26 
##          0          0          0          0          0          0 
## 2012-10-27 2012-10-28 2012-10-29 2012-10-30 2012-10-31 2012-11-02 
##          0          0          0          0          0          0 
## 2012-11-03 2012-11-05 2012-11-06 2012-11-07 2012-11-08 2012-11-11 
##          0          0          0          0          0          0 
## 2012-11-12 2012-11-13 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
##          0          0          0          0          0          0 
## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 
##          0          0          0          0          0          0 
## 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##          0          0          0          0          0
```

##### The mean of the total number of steps taken per day are 0.4375, 39.4166667, 42.0694444, 46.1597222, 53.5416667, 38.2465278, 44.4826389, 34.375, 35.7777778, 60.3541667, 43.1458333, 52.4236111, 35.2048611, 52.375, 46.7083333, 34.9166667, 41.0729167, 36.09375, 30.6284722, 46.7361111, 30.9652778, 29.0104167, 8.6527778, 23.5347222, 35.1354167, 39.7847222, 17.4236111, 34.09375, 53.5208333, 36.8055556, 36.7048611, 36.2465278, 28.9375, 44.7326389, 11.1770833, 43.7777778, 37.3784722, 25.4722222, 0.1423611, 18.8923611, 49.7881944, 52.4652778, 30.6979167, 15.5277778, 44.3993056, 70.9270833, 73.5902778, 50.2708333, 41.0902778, 38.7569444, 47.3819444, 35.3576389, 24.46875. See above for a better formatted output along with dates.

##### The median of the total number of steps taken per day are 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.See above for a better formatted output along with dates.

### Part 3: What is the average daily activity pattern?


```r
#Assignment Part 3 step 1: Time series plot of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all days (y-axis)

interval.factor.good <- factor(fit.steps.good$interval)
interval.mean.good <- tapply(fit.steps.good$steps, interval.factor.good, mean)

# Make a time-series plot
pdf("TimeSeriesAvgStepsNAIgnored.pdf")
opar<-par(no.readonly=TRUE)
par(lty=1, pch=17)
plot(fit.steps.good$interval[1:288], interval.mean.good, type = "l", main = "Time Series Plot",ylab ="Average No. of Steps" ,xlab="5 min time intervals")
par(opar)
dev.off()
```

```
## RStudioGD 
##         2
```
##### The Time Series Plot was saved to TimeSeriesAvgStepsNAIgnored.pdf


```r
#Assignment Part 3 step 2: Find and report the max. number of steps
steps.max.good = max(interval.mean.good)
a<- interval.mean.good
b<- which(a==max(a))
time.at.max <- fit.steps.good$interval[b]
time.at.max
```

```
## [1] 835
```

```r
steps.max.good
```

```
## [1] 206.1698
```
##### The maximum number of steps is 206.1698113 at this 5-minute interval: 835.

### Part 4: Imputing missing values


```r
# Part 4, step 1: Calculate and report the total # of missing values in the dataset
missing.flag <- is.na(fit.steps$steps)
no.missing <- table(missing.flag)["TRUE"]
no.missing 
```

```
## TRUE 
## 2304
```

##### The number of missing values in the dataset are 2304.


```r
#Part 4, part 2: Devise a strategy to replace NAs in the dataset
# Replaced the missing values with the mean for the 5 minute interval

# First, tried replacing with the day average. This was not meaningful - day long data were missing
fit.steps.good <- na.omit(fit.steps)
# There were only 53 levels now.
date.factor.good <- factor(fit.steps.good$date)
day.mean.good <- tapply(fit.steps.good$steps, date.factor.good, mean)
# so, there were 8 days with no activity at all. So, this is not a good way
# to find means to plug into na slots.

# Second, tried to find the mean for a given 5 minute interval and may be there will be
# no na factors there (that is, it will return 288 both times)
# This was verified to be the case.
# So, mean for 5 minute intervals are calculated and are used in place of NAs 
interval.factor <- factor(fit.steps$interval)
interval.factor.good <- factor(fit.steps.good$interval)
interval.mean.good <- tapply(fit.steps.good$steps, interval.factor.good, mean)
```
##### Strategy used: Use the mean for 5-minute intervals to substitite for missing values. 


```r
#Part 4, step 3: create a new dataset 
fit.index <- !is.na(fit.steps$steps)
# if fit.index is FALSE, change the NA to mean value for that interval
fs <- fit.steps
img <- interval.mean.good
# add img to fs data set. img is repeated for all the data sets
fs$mean <- img
# Just to compare and contrast, create another array - 'step' and update the
# NAs based on the index flag. All other values should remain the same. 
fs$step[!fit.index]<- fs$mean[!fit.index]
#Verified that the numbers are OK for the case of 3

new.fit.steps <- fit.steps
new.fit.steps$steps <- fs$step
```
##### Created a new dataset above that is equal to the orignial dataset but with the missing data filled in.


```r
# Part 4, step 4: Make a histogram and calculate mean and median. Are they
# different?

summary (new.fit.steps$date)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##        288        288        288        288        288        288 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##        288        288        288        288        288        288 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##        288        288        288        288        288        288 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##        288        288        288        288        288        288 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##        288        288        288        288        288        288 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##        288        288        288        288        288        288 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##        288        288        288        288        288        288 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##        288        288        288        288        288        288 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##        288        288        288        288        288        288 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##        288        288        288        288        288        288 
## 2012-11-30 
##        288
```

```r
new.date.factor <- factor(new.fit.steps$date)
# Assignment, Part 4, Step 4a: Find total number of steps taken per day
new.day.sum<-tapply(new.fit.steps$steps, new.date.factor, sum)
new.day.sum
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##   10766.19     126.00   11352.00   12116.00   13294.00   15420.00 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##   11015.00   10766.19   12811.00    9900.00   10304.00   17382.00 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##   12426.00   15098.00   10139.00   15084.00   13452.00   10056.00 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##   11829.00   10395.00    8821.00   13460.00    8918.00    8355.00 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##    2492.00    6778.00   10119.00   11458.00    5018.00    9819.00 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##   15414.00   10766.19   10600.00   10571.00   10766.19   10439.00 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##    8334.00   12883.00    3219.00   10766.19   10766.19   12608.00 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##   10765.00    7336.00   10766.19      41.00    5441.00   14339.00 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##   15110.00    8841.00    4472.00   12787.00   20427.00   21194.00 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##   14478.00   11834.00   11162.00   13646.00   10183.00    7047.00 
## 2012-11-30 
##   10766.19
```

```r
# Assignment, Part 4, Step 4b: Make a histogram of the total number of steps taken each day"
pdf("HistogramStepsNAFilled.pdf")
hist(new.day.sum,breaks=100,col='red', xlab="Total Steps Per Day", main='Histogram')
dev.off()
```

```
## RStudioGD 
##         2
```

```r
# assignment, Part 4, sub part 4: Calculate mean and median. Are they diffeent?
summary(new.fit.steps)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 27.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##                   (Other)   :15840
```

```r
summary(fit.steps)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
# No effect since the average is added to the NA and avearged. 
# As an example, With 10 values at 1, the average is 1. Now replace one NA with 1. 
#Take the new average for 11 values of 1 each. It is still 1. 
```
##### Total daily number of steps remains the same, as explained above in comments

### Part 5: Are there differences in activity patterns between weekdays and weekends?


```r
# set up a vector of weekday (false) or weekend
#remove factor
date.char <- as.character(new.fit.steps$date)
step.date <- as.Date(date.char,'%Y-%m-%d')
# step.date
head(step.date)
```

```
## [1] "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01"
## [6] "2012-10-01"
```

```r
tail(step.date)
```

```
## [1] "2012-11-30" "2012-11-30" "2012-11-30" "2012-11-30" "2012-11-30"
## [6] "2012-11-30"
```

```r
day.name <- weekdays(step.date)
# day.name
head(day.name)
```

```
## [1] "Monday" "Monday" "Monday" "Monday" "Monday" "Monday"
```

```r
tail(day.name)
```

```
## [1] "Friday" "Friday" "Friday" "Friday" "Friday" "Friday"
```

```r
new.fit.steps$day.name <- day.name
# set up a flag -- to distinguish weekdays from weekend
wkend1 <- "Saturday"
wkend2 <- "Sunday"
weekend.flag <- (day.name == wkend1) | (day.name == wkend2)
head(weekend.flag)
```

```
## [1] FALSE FALSE FALSE FALSE FALSE FALSE
```

```r
tail(weekend.flag)
```

```
## [1] FALSE FALSE FALSE FALSE FALSE FALSE
```

```r
new.fit.steps$weekend.flag <- weekend.flag
#split based on weekday (FALSE) or weekend day (TRUE)

#calculate 5 minute interval means for weekdays and weekend days separately

new.fit.steps.weekday <- new.fit.steps[which (new.fit.steps$weekend.flag == "FALSE"),]
new.interval.weekday.factor  <- factor(new.fit.steps.weekday$interval)
new.fit.steps.weekend <- new.fit.steps[which (new.fit.steps$weekend.flag == "TRUE"),]
new.interval.weekend.factor  <- factor(new.fit.steps.weekend$interval)
new.interval.mean.weekday  <- tapply(new.fit.steps.weekday$steps, new.interval.weekday.factor, mean)
new.interval.mean.weekend  <- tapply(new.fit.steps.weekend$steps, new.interval.weekend.factor, mean)
```
##### Two sets of data for weekday and weekend day are obtained (see above)
#####Hello


```r
#plot both weekday and weekend separately
opar<-par(no.readonly=TRUE)
par(lty=1, pch=17)
split.screen(c(2,1))
```

```
## [1] 1 2
```

```r
#set.panel(2,1)
pdf("TimeSeriesWeekdayWeekendStepsNAFilled.pdf")
screen(1)
```

```
## [1] FALSE
```

```r
plot(fit.steps.good$interval[1:288], new.interval.mean.weekday, type = "l", main = "Weekday Time Series Plot",ylab ="Average No. of Steps" ,xlab="5 min time intervals")
screen(2)
```

```
## [1] FALSE
```

```r
plot(fit.steps.good$interval[1:288], new.interval.mean.weekend, type = "l", main = "Weekend Time Series Plot",ylab ="Average No. of Steps" ,xlab="5 min time intervals")
dev.off()
```

```
## RStudioGD 
##         2
```

```r
#set.panel()
par(opar)
```

![plot of chunk part5step2](figure/part5step2-1.png) 
##### Two panel plots for weekday and weekend days is saved in TimeSeriesWeekdayWeekendStepsNAFilled.pdf (two plots). Much more activity during the whole weekend day(s) is noted. 

