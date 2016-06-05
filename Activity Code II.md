Reproducible Research Project I - Analyzing FitBit Data

Joshua Peterson
June, 2016
===============================



This was the first project for the Reproducible Research course in Coursera's Data Science specialization track. The purpose of the project was to answer a series of questions using data collected from a FitBit.</p>

### Load data

#### Read csv data file

activity_data<- read.csv("activity.csv")


### Problem 1: What is the mean total number of steps taken per day?

#### Sum Steps by Day|Create Histogram|Calculate Mean and Median

steps_day<- aggregate(steps ~ date, activity_data, sum)
hist(steps_day$steps, main = paste("Total Steps Each Day"), col="blue", xlab

paste("Mean Steps/Day=", mean(steps_day$steps))
paste("Median Steps/Day=", median(steps_day$steps))



### Problem 2: What is the average daily activity pattern?

1. Calculate average steps for each interval for all days.
2. Plot the Average Number of Steps/Day by Interval
3. Find interval with most average steps

steps_interval<- aggregate(steps ~ interval, activity_data, mean)

plot(steps_interval$interval, steps_interval$steps, type="l", xlab="Interval", ylab="Number of Steps", main="Average Number of Steps/Day by Interval")


#### Interval with max number of steps

max_interval<- steps_interval[which.max(steps_interval$steps),1]
max_interval



### Problem 3: Impute missing values and compare to non-imputed data.

#### Missing data needed to be imputed

incomplete_data<- sum(!complete.cases(activity_data))
imputed_data<- transform(activity_data, steps = ifelse(is.na(activity_data$steps), steps_interval$steps[match(activity_data$interval, steps_interval$interval)], activity_data$steps))



#### Zeros imputed for start date of 10.01.2012.  Would have been 9,000 steps higher the proceeding day of 126 steps.  NAs assumed to be 0 to fit trend.

imputed_data[as.character(imputed_data$date) == "2012.10.01", 1] <-0



#### Re-count total steps by day and create histogram.

steps_day_II<- aggregate(steps ~ date, imputed_data, sum)
hist(steps_day_II$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
hist(steps_day$steps, main= paste("Total Steps Each Day"), col="red", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non_imputed"), col=c("blue", "red"), lwd=10)



#### New mean and median for imputed data.

paste("Imputed Data Mean Steps/Day=", mean(steps_day_II$steps))
paste("Imputed Data Median Steps/Day=", median(steps_day_II$steps))



#### Total difference

paste("Total difference=", sum(steps_day_II$steps) - sum(steps_day$steps))



### Problem 4: Are there differnces in activity patterns between weekdays and weekends?

#### Create plot to compare and constrast activity on weekdays and weekends.

weekdays<- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)), weekdays), "Weekday", "Weekend"))
steps_interval_II<- aggregate(steps ~ interval + dow, imputed_data, mean)
library(lattice)
xyplot(steps_interval_II$steps ~ steps_interval_II$interval|steps_interval_II$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")


