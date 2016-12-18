---
title: "Course5Project1"
author: "Andrea Heide"
date: "18 Dezember 2016"
output: html_document
---

# Reproducible Research Assignment 1  

## Loading and preprocessing the data   

##### 1. Load the data:
```{r echo=TRUE}
ActivityData <- read.csv("activity.csv", na.strings = "Not Available", stringsAsFactors = FALSE) 
```

##### 2. Process and transform the data for analysis:
```{r echo=TRUE}
## Format column date (character) as date
ActivityData$date <- as.Date(ActivityData$date)
## Format column steps as numeric
ActivityData$steps <- as.numeric(ActivityData$steps)
```
##  
## What is the mean total number of steps taken per day?  

##### 1. Calculate the total number of steps taken per day:
```{r echo=TRUE}
TotalStepsPerDay <- aggregate(ActivityData$steps, by=list(Category=ActivityData$date), FUN=sum)
colnames(TotalStepsPerDay)[1] <- "date"
colnames(TotalStepsPerDay)[2] <- "steps"
```

##### 2. Make a histogram of the total number of steps taken each day:
```{r echo=TRUE}
hist(TotalStepsPerDay$steps,
main="Histogram Total Number of Steps Each Day", 
xlab = "Total Number Steps per Day",
ylab="Count",
border="blue", 
col="grey",
las=1, 
breaks=5)
```

##### 3. Calculate and report the mean and median of the total number of steps taken per day:
```{r echo=TRUE}
options(scipen=999)
MeanSteps <- mean(TotalStepsPerDay$steps, na.rm = TRUE)
MedianSteps <- median(TotalStepsPerDay$steps, na.rm = TRUE)
```
  
The mean of the total number of steps taken per day is **`r MeanSteps`**.  
The median of the total number of steps taken per day is **`r MedianSteps`**.  


## What is the average daily activity pattern?  

##### 1. Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days:
```{r echo=TRUE}
## Calculate average per interval
library(plyr)
MeanStepsPerInterval <- aggregate(steps ~ interval, data = ActivityData, mean)
```
```{r echo=TRUE}
## Plot time series
plot(MeanStepsPerInterval$interval, MeanStepsPerInterval$steps , xlab= "5-minute interval", ylab= "average number of steps", type='l', col='red') 
title(main="Average Daily Activity Pattern", col.main="black", font.main=4)
```

##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r echo=TRUE}
MaxInterval <- MeanStepsPerInterval[(MeanStepsPerInterval$steps == max(MeanStepsPerInterval$steps)), 1]
```

The **`r MaxInterval` interval**  contains the maximum number of steps, on average across all days.  


## Imputing missing values  

##### 1. Calculate and report the total number of missing values in the dataset 
```{r echo=TRUE}
## Total number NA
countNA <- sum(is.na(ActivityData$steps))
```

The total number of missing values (NA) in the dataset is **`r countNA`**.    

##### 2. Devise a strategy for filling in all of the missing values in the dataset 
Strategy for filling in missing values: 
*Use the mean of the corresponding 5 minute -interval across all days to fill in the missing interval.*  

##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r echo=TRUE}
## Create new dataset "Fill"
ActivityDataFill <- ActivityData

## Replace missing values
for (i in 1:nrow(ActivityDataFill)) {
        if (is.na(ActivityDataFill[i, 1]) == TRUE) {
                Interval <- ActivityDataFill[i, 3]
                MeanInterval <- MeanStepsPerInterval[(MeanStepsPerInterval$interval == Interval),2]
                ActivityDataFill[i, 1] <- MeanInterval
        }
}
```

##### 4. Analysis of the effect of filling in missing data:  

###### Histogram of the total number of steps taken each day:
```{r echo=TRUE}
## Calculate total number of steps 
TotalStepsPerDayFill <- aggregate(ActivityDataFill$steps, by=list(Category=ActivityDataFill$date), FUN=sum)
colnames(TotalStepsPerDayFill)[1] <- "date"
colnames(TotalStepsPerDayFill)[2] <- "steps"
```

```{r echo=TRUE}
## Plot histogram
hist(TotalStepsPerDayFill$steps,
     main="Histogram Total Number of Steps Each Day, NA Replaced", 
     xlab = "Total Number Steps per Day",
     ylab="Count",
     border="blue", 
     col="grey",
     las=1, 
     breaks=5)
```

###### Calculate and report mean and medianof the total number of steps taken per day:
```{r echo=TRUE}
options(scipen=999)
MeanStepsFill <- mean(TotalStepsPerDayFill$steps)
MedianStepsFill <- median(TotalStepsPerDayFill$steps)
```

The mean of the total number of steps taken per day without missing values is **`r MeanStepsFill`**.  
The median of the total number of steps taken per day without missing values is **`r MedianStepsFill`**.  

###### Do these values differ from the first question (without filling in missing values)?

Function | Calculated with missing values | Calculated without missing values
-------- | ------------------------------ | ----------------------------------
Mean     |  `r MeanSteps`                 |  `r MeanStepsFill`
Median   |  `r MedianSteps`               |  `r MedianStepsFill`

*There is only a very slight difference compared to the mean and median calculated with missing values and it only concerns the median.*  

###### What is the impact of imputing missing data?
*The impact is very limited, because a method was chosen to replace NA values per 5-minute interval by the corresponding mean.*  


## Are there differences in activity patterns between weekdays and weekends?  

### 1. Create a new factor variable in the dataset with two levels “weekday” and “weekend” indicating whether a given date is a weekday or weekend day
```{r echo=TRUE}
Sys.setenv(LANG = "en")
ActivityDataFill$day <- ifelse(weekdays(ActivityDataFill$date) %in% c("Saturday", "Sunday"),"weekend", "weekday")
```

### 2. Make a panel plot containing a time series plot of the 5-minute interval  and the average number of steps taken, averaged across all weekday days or weekend days  
```{r echo=TRUE}
## Calculate average number of steps 
library(plyr)
MeanStepsPerIntervalFill <- aggregate(steps ~ interval + day, data = ActivityDataFill, mean)
```

```{r echo=TRUE}
## Plot average steps per interval, weekend and weekdays, using lattice
library(lattice)
xyplot(steps ~ interval | factor(day), data=MeanStepsPerIntervalFill,
        type = "l",
        xlab = "Interval",
        ylab = "Number of Steps",
        main = "Average Number of Steps per Interval",
        layout = c(1,2))

```


*On weekdays, there is a more pronounced maximum in the number of steps around the 800 interval, followed by less activity than in the weekeind. In the weekend, the average activty is higher, but without the clear maximum around the 800 interval. A hypothesis could therefore be that this peak is related to work.*
