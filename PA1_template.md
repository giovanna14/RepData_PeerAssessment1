---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Synopsis

In this report we consider data collected from a personal activity 
monitoring device. The number of steps of an anonymous individual
were collected at time intervals of 5 minutes during the months of
October and November, 2012. The data are available as a
zipped .csv file in this same repository. We briefly analyse the 
amount and average distribution of activity across the day and 
the activity difference between weekdays and weekend days.

## Loading and preprocessing the data


```r
## unzip the datafile into a temporary directory 
## and read it into an R data frame called mydata
zipdir <- tempfile()
dir.create(zipdir)
unzip("repdata_data_activity.zip", exdir=zipdir)
files <- list.files(zipdir)
files <- files[grep("\\.csv$", files)] 
myfile <- file.path(zipdir,files[[1]])
mydata <- read.csv(myfile)
head(mydata,3)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
```


## What is the mean total number of steps taken per day?


```r
## total number of steps taken each day
sum_per_date <- tapply(mydata$steps,mydata$date,sum)
dim(sum_per_date)
```

```
## [1] 61
```

```r
hist(sum_per_date,breaks="FD",
     main="Total number of daily steps",
     xlab = "Total steps per day",
     col = "white",
     border = "red")
```

![plot of chunk originaldata](figure/originaldata-1.png) 

```r
## mean and median number of steps per day
mean_med <- c(summary(sum_per_date)[4],summary(sum_per_date)[3])
mean_med
```

```
##   Mean Median 
##  10770  10760
```

In a period of 61 days (October and November 2012),
the mean and median total number of steps taken per day were
10770 and 10760, 
respectively.

## What is the average daily activity pattern?


```r
# calculate and plot the average number of steps per each time interval
# (excluding missing values) across the whole monitoring period.
mean_per_int <- aggregate(mydata$steps[!is.na(mydata$steps)],
                          list(interval = mydata$interval[!is.na(mydata$steps)]),
                          mean)
# convert the interval identifier into hh:mm time
x <- mean_per_int$interval
y <- rep(100*(0:23),each=12)
time <- as.POSIXct(strptime(paste0(y/100,":",(x - y)), "%R")) 
# use new time variable for plot
with(mean_per_int, plot(time,x,type="l",
                        main="Average daily activity pattern",
                        ylab = "Mean steps per time interval",
                        xlab = "Time in 5 minutes intervals",
                        col = "purple"
                        )
     )
```

![plot of chunk avg_activity](figure/avg_activity-1.png) 

```r
max_int <- mean_per_int$interval[mean_per_int$x >= max(mean_per_int$x)]
max_int
```

```
## [1] 835
```

On average, across all the days in the dataset, the maximum number of steps
occurs in the time intervals 835, i.e. at 8:35 a.m. (assuming that 
the time interval are counted starting at midnight).

## Imputing missing values


```r
# number of complete observations in the dataset
cc <- complete.cases(mydata)
# number of NAs in the dataset
n_NA <- sum(!cc)
n_NA
```

```
## [1] 2304
```

```r
list_missing_val <- tapply(mydata$steps, mydata$date, is.na)
missing_val_per_day <- sapply(list_missing_val,sum)
missing <- missing_val_per_day[missing_val_per_day > 0]
missing
```

```
## 2012-10-01 2012-10-08 2012-11-01 2012-11-04 2012-11-09 2012-11-10 
##        288        288        288        288        288        288 
## 2012-11-14 2012-11-30 
##        288        288
```
The number of NAs in the dataset is 2304, namely 288 NAs
for each of 8 days, during which no valid measurement 
was taken (there are 288 five-minute intervals in a day).  
We decided to fill in missing values in these days by substituting 
for each 5-minute time interval the mean total number of steps taken 
in that interval. 
We investigate the impact of this strategy of imputing missing data by
calculating the histogram of total daily steps and the mean and median
total number of steps taken per day from the modified dataset and
comparing it with those obtained from the original dataset.


```r
# create a new data frame in which missing values in a given time
# interval is substituted with the average number of steps in that 
# time interval 
newdata <- merge(mydata,mean_per_int,sort=FALSE)
newdata$steps[is.na(newdata$steps)] <- newdata$x[is.na(newdata$steps)]
newdata <- newdata[,1:3]
# histogram of total daily steps for the new dataset
newsum_per_date <- tapply(newdata$steps,newdata$date,sum)
hist(newsum_per_date,breaks="FD",
     main="Total number of daily steps",
     xlab = "Total steps per day",
     col = "lightgreen",
     border = "darkgreen",
     lwd=3)
# overplot histogram of original data for comparison
hist(sum_per_date,breaks="FD",
     col=NULL,
     border="red",
     lty=2,
     add=TRUE)
legend("topright",c("imputed data","original data"),
       col=c("darkgreen","red"),
       lty=c(1,2),lwd=c(3,1),bty="n"
       )
```

![plot of chunk simulatedata](figure/simulatedata-1.png) 

```r
# some summary statistics of the imputed data set
newmean_med <- c(summary(newsum_per_date)[4],summary(newsum_per_date)[3])
newmean_med
```

```
##   Mean Median 
##  10770  10770
```

As it can be seen in the above graph, the histogram after imputing 
missing data shows a similar distribution of the total number of steps per 
day to the original data, however the 
peak of the distribution is higher after imputing data with the strategy 
described above. There was no effect on the mean of the distribution 
that remained unchanged, while the median value has slightly increased
and is now equal to the mean value.

## Are there differences in activity patterns between weekdays and weekends?



```r
# create a factor variable with level "weekday" for dates that 
# correspond to weekdays and level "weekend" for dates that correspond
# to weekend and add this variable to the imputed dataset
wd <- as.factor(weekdays(as.Date(newdata$date),abbreviate=TRUE))
levels(wd)[levels(wd) %in% c("Sat", "Sun")] <- c("weekend")
levels(wd)[levels(wd) != "weekend"] <- c("weekday")
newdata$weekdays <- wd
# mean steps per time interval in weekdays
mean_per_int_wd <- aggregate(newdata$steps[newdata$weekdays == "weekday"],
                          list(interval = newdata$interval[
                                newdata$weekdays == "weekday"]),
                          mean)
names(mean_per_int_wd) <- c("interval","meansteps_per_interval")
# add a column with time in hh:mm
mean_per_int_wd$time <- time
# mean steps per time interval in weekend days
mean_per_int_we <- aggregate(newdata$steps[newdata$weekdays == "weekend"],
                          list(interval = newdata$interval[
                                newdata$weekdays == "weekend"]),
                          mean)
names(mean_per_int_we) <- c("interval","meansteps_per_interval")
# add a column with time in hh:mm
mean_per_int_we$time <- time
# put results in a single dataframe with weekdays factor
mean_per_int_wd$weekdays <- factor("weekday")
mean_per_int_we$weekdays <- factor("weekend")
new_mean_per_int <- rbind(mean_per_int_wd,mean_per_int_we)
# two-panel plot
library(lattice)
xyplot(meansteps_per_interval ~ time | weekdays, 
       data = new_mean_per_int, layout = c(1,2),
       xlab="Time in 5 minute intervals",
       ylab="Mean number of steps per time interval",
       type="l",lty=1,col="purple",
       scales=list(x=list(format="%H:%M"),
                   alternating=c(1,1),
                   tck=c(1,0),
                   tick.number=8,
                   axs="i")
       )
```

![plot of chunk weekdays_activity](figure/weekdays_activity-1.png) 

When considering weekdays and weekend days separately,
it appears that during week days the activity concentrates especially 
in the early morning hours starting around 5:30 a.m. with a peak 
around 9 a.m. and with only little activity after 8 p.m.  
In the weekends the activity is more homogeneously spread across 
day-time, mostly between 8 a.m. and 9 p.m.

