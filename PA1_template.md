---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

# Reproducible Research: Peer Assessment 1

<!-- 
  Note:  For consistency, don't use RStudio "Knit HTML".  Instead, in the R
  Console, run the command:  knit2html("PA1_template.Rmd")
  -->

## Loading and preprocessing the data

1. Show any code that is needed to load the data
2. Show any code that is needed to process/transform the data (if necessary)
   into a format suitable for your analysis


```r
# The assignment said, "feel free to use any plotting system in R", I kept it
# simple and stuck with 1) base graphics for histograms, and 2) lattice graphics
# for panel plots.
library(lattice)
library(dplyr)
```

```r
# Unzip the data into a "data" folder which GIT will ignore (see the file .gitignore)
# We specify the column classes in advance, and then convert the interval from
# an integer to a human-readable factor.
unzip("activity.zip", exdir = "data")
activity <- read.csv("data/activity.csv", colClasses=c("integer", "Date", "integer"))
activity$interval <- as.factor(sub("(\\d\\d)(\\d\\d)", "\\1:\\2", 
                         sprintf("%4.4d", activity$interval)))
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day
2. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day


```r
# Calculate daily sum, then the mean and median of the sums (so we can compare, later)
dailySum <- activity %>% group_by(date) %>%
            summarise_each(funs(sum(., na.rm = TRUE)), steps)
dailyMean <- mean(dailySum$steps)
dailyMedian <- median(dailySum$steps)

# Histogram of total number of steps taken per day
hist(dailySum$steps, breaks=10, xlab="Steps", main="Total steps taken per day")
```

![plot of chunk totalStepsPerDay](figure/totalStepsPerDay-1.png) 

```r
# That's a big bump at zero ... how many are exactly 0.0?
zeroDays <- sum(dailySum$steps == 0)
totalDays <- length(dailySum$steps)
print(paste(zeroDays, " of ", totalDays, " days (", 
            round(100 * zeroDays / totalDays, 1), "%) have no steps at all",
            sep=""))
```

```
## [1] "8 of 61 days (13.1%) have no steps at all"
```

```r
# Print the mean and median values (rounded to one decimal place)
print(paste("Mean of daily sum:", round(dailyMean, 1)))
```

```
## [1] "Mean of daily sum: 9354.2"
```

```r
print(paste("Median of daily sum:", round(dailyMedian, 1)))
```

```
## [1] "Median of daily sum: 10395"
```

## What is the average daily activity pattern?

1. Make a time series plot of the 5-minute interval (x-axis) and the average
   number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset,
   contains the maximum number of steps?


```r
intervalMeans <- activity %>% group_by(interval) %>%
                 summarise_each(funs(mean(., na.rm = TRUE)), steps)

xTickAt = seq(1, 12*24, by=12) # one label every hour on the x axis
xLabels = intervalMeans$interval[xTickAt]
p <- xyplot(steps ~ interval, data=intervalMeans, type="l",
            main="Average # steps per 5 minute interval",
            xlab="5-minute Interval", ylab="Average Number of steps",
            scales=list(x=list(at=xTickAt, labels=xLabels), rot=90))
print(p)
```

![plot of chunk dailyActivity](figure/dailyActivity-1.png) 

```r
maxEntry <- intervalMeans[which.max(intervalMeans$steps),]
print(paste("5-minute interval with the maximum average number of steps: ", 
            maxEntry$interval, " with ", round(maxEntry$steps, 1), " steps",
            sep=""))
```

```
## [1] "5-minute interval with the maximum average number of steps: 08:35 with 206.2 steps"
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values. The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset
2. Devise a strategy for filling in all of the missing values in the dataset.
3. Create a new dataset that is equal to the original dataset but with the
   missing data filled in.
4. Make a histogram of the total number of steps taken each day
5. calculate and report the mean and median total number of steps taken per day.

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
naValues <- sum(is.na(activity$steps))
print(paste("Number of NA values:", naValues, "out of", nrow(activity)))
```

```
## [1] "Number of NA values: 2304 out of 17568"
```

```r
# "Group by" interval so the "mean" will calculate along that variable
# This replaces any NA value with the average value of all non-NA values
# for the same time interval.
newActivity <- activity %>% group_by(interval) %>%
  mutate(steps = ifelse(is.na(steps), mean(steps,na.rm=TRUE), steps))

newDailySum <- newActivity %>% group_by(date) %>%
                summarise_each(funs(sum(., na.rm = TRUE)), steps)
newDailyMean <- mean(newDailySum$steps)
newDailyMedian <- median(newDailySum$steps)

# Histogram of corrected total number of steps taken per day
hist(newDailySum$steps, breaks=10, xlab="Steps", 
     main="Total steps taken per day (corrected)")
```

![plot of chunk missingValues](figure/missingValues-1.png) 

```r
# Compare corrected and uncorrected histograms
transRed <- rgb(1, 0, 0, 0.6)
transBlue <- rgb(0, 0, 1, 0.6)
hist(newDailySum$steps, breaks=10, xlab="Steps", 
     main="Total steps taken per day (corrected vs uncorrected)", col=transRed)
hist(dailySum$steps, breaks=10, add=TRUE, col=transBlue,)
legend("topright", legend=c("Corrected","Uncorrected"), 
       fill=c(transRed, transBlue))
```

![plot of chunk missingValues](figure/missingValues-2.png) 

```r
# After imputation, do we still have a lot of days with 0.0 steps?
newZeroDays <- sum(newDailySum$steps == 0)
print(paste("After imputation, ", newZeroDays, " of ", totalDays, " days (", 
            round(-100 * newZeroDays / totalDays, 1), "%) have no steps at all",
            sep=""))
```

```
## [1] "After imputation, 0 of 61 days (0%) have no steps at all"
```

```r
if (zeroDays > 0) {
  reduction <- round(-100 * (newZeroDays - zeroDays) / zeroDays, 1)
  print(paste("This is a reduction of ", reduction, "%", sep=""))
}
```

```
## [1] "This is a reduction of 100%"
```

```r
# Print the corrected mean and median values (rounded to one decimal place)
# Also compare to our original computations with missing values
meanDiff <- round((newDailyMean - dailyMean) / dailyMean * 100, 1)
print(paste("Mean of corrected daily sum: ", round(newDailyMean, 1),
            ", (Compare to ", round(dailyMean, 1), "), a change of ",
            meanDiff, "%",
            sep=""))
```

```
## [1] "Mean of corrected daily sum: 10766.2, (Compare to 9354.2), a change of 15.1%"
```

```r
medianDiff <- round((newDailyMedian - dailyMedian) / dailyMedian * 100, 1)
print(paste("Median of corrected daily sum: ", round(newDailyMedian, 1),
            ", (Compare to ", round(dailyMedian, 1), "), a change of ",
            medianDiff, "%",
            sep=""))
```

```
## [1] "Median of corrected daily sum: 10766.2, (Compare to 10395), a change of 3.6%"
```

## Are there differences in activity patterns between weekdays and weekends?

Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels.
2. Make a panel plot containing a time series plot of the 5-minute interval
   (x-axis) and the average number of steps taken, averaged across all weekday
   days or weekend days (y-axis). See the
   [README](./instructions_fig/sample_panelplot.png) file to see an example 
   using simulated data of what this plot should look like.


```r
dayTypeColumn <- ifelse(weekdays(newActivity$date) %in% c("Saturday","Sunday"),
                        "Weekend", "Weekday")
dayTypeIntervalMeans <- newActivity %>% ungroup() %>%
    mutate(dayType = as.factor(dayTypeColumn)) %>%
    group_by(interval, dayType) %>%
    summarise_each(funs(mean(., na.rm = TRUE)), steps)

#########
# FIXME: Due to interval being a factor, the X axis labels get screwed up, even
# though the plot itself is correct.  Changing the xTickAt works around the
# issue, but it's a kludge.  Would like something better even though that kludge
# works perfectly.
#########
xTickAt = seq(1, 12*24*2, by=24) # one label every hour on the x axis
xLabels = dayTypeIntervalMeans$interval[xTickAt]
xTickAt = seq(1, 12*24, by=12) # one label every hour on the x axis
p <- xyplot(steps ~ interval | levels(dayType), data=dayTypeIntervalMeans, type="l",
            layout=c(1,2), main="Average # steps per 5 minute interval",
            xlab="5-minute Interval", ylab="Average Number of steps",
            scales=list(x=list(at=xTickAt,labels=xLabels), rot=90))
print(p)
```

![plot of chunk weekendWeekdayDifferences](figure/weekendWeekdayDifferences-1.png) 

<!-- Clean temporary variables from the environment -->

