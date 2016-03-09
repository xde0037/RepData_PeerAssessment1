------
# title: "Reproducible Research: Project 01 "
# output: HTML Document
#    keep_md: true
------

--- Load libraries
library(knitr)
opts_chunk$set(tidy = TRUE)


# [1] : Loading and preprocessing the data

   --- We will load the CSV File, assume that you have setwd to <br>
   --- your local path where this CSV file is located.  <br>

   setwd("C:/dscience")
   activity <- read.csv("activity.csv")

---   we will pre-process this file as follows: <br>
---   1 : Convert date column to the POSIX date format.: YYYY-MM-DD <br>
---   2 : Convert the integer column to a new POSIX time column in HH:MM:SS <br>

--- convert character date to POSIX date <br>

activity$date <- as.POSIXct(strptime(activity$date, "%Y-%m-%d"), tz = "")

--- first convert integer time to character and pad with leading zeros...

activity$time <- sprintf("%04d", activity$interval)  

--- ...then convert to the date type

activity$time <- as.POSIXct(activity$time, "%H%M", tz = "")

--- Let's display some of the data and the structure of the data frame <br>
--- after pre-processing. <br>

str(activity) <br>
--- 'data.frame':    17568 obs. of  4 variables: <br>

---  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...<br>
---  $ date    : POSIXct, format: "2012-10-01" "2012-10-01" ...<br>
---  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...<br>
---  $ time    : POSIXct, format: "2014-11-22 00:00:00" "2014-11-22 00:05:00" ...<br>
<br>
<br>

# [2] : What is mean total number of steps taken per day? <br>
--- To answer this question, we will use aggregate() function , create a new <br>
--- data frame with 2 columns : Date and Total Steps on each date: <br>
<br>
<br>


total_steps_by_date <- aggregate(x=list(total_steps=activity$steps), by=list(date=activity$date), FUN=mean, na.rm=TRUE)


--- The histograms can be produced now to show the distribution of total steps: <br>
--- In order to avoid waring of Hebrew characters occurs, use windows() <br>
--- function to set width and height in advance <br>

options(device = function(file, width = 7, height = 7, ...) {
    windows(width = width, height = height, ...)
})

par(mfrow = c(1, 2))  # 1 row, 2 columns <br>
png("plot1.png")
 --- frequencies   <br><br>
hist(total_steps_by_date$total_steps, breaks = 30, xlab = "Total Steps", main = "Total Steps Per Day", col = "lightblue") <br>

dev.off()
---  desnsity <br>

plot(density(total_steps_by_date$total_steps, na.rm = TRUE), xlab = "Total Steps", ylab = "Density", main = "Total Steps Per Day", col = "purple", lwd = 3) <br>
<br>

par(mfrow = c(1, 1))  <br>

dev.off()

--- Finally, we'll calculate the mean and median number of steps per day.  <br>

mean(total_steps_by_date$total_steps) <br>
--- [1] 9354.23

median(total_steps_by_date$total_steps, na.rm = T) <br>
--- [1] 10395


# [3] : What is the average daily activity pattern?  <br>
<br>

--- First we'll use the aggregate() function to obtain the average number <br>
--- of steps for each time interval. With the result, we can draw a time  <br>
--- series plot showing time interval on the x-axis and mean number of <br>
--- steps for the time interval on the y-axis. <br>
<br>
png("plot3.png")
average_steps_by_time <- aggregate(list(average_steps = activity$steps), by = list(time = activity$time,  interval = activity$interval), FUN = mean, na.rm = TRUE) <br>
plot(average_steps ~ time, data = average_steps_by_time, xlab = "Time interval",     ylab = "Mean steps", main = "Mean Steps By Time Interval", type = "l",
     col = "blue", lwd = 2) 
dev.off()
	 <br>
<br>
--- Next, we determine the time interval with the maximum average number <br>
--- of steps. <br>
<br>
average_steps_by_time[which.max(average_steps_by_time$average_steps), ]  <br>

--- time interval average_steps  <br>
--- 104 2014-11-22 08:35:00      835      206.1698  <br>
<br>
<br>



# [4] : Imputing missing values  <br>

--- First get the count of total missing values in STEPS column:  <br>

sum(is.na(activity[, "steps"]))   <br>
--- [1] 2304    <br>

--- For the missing values we will use average number of steps for   <br>
--- time interval as we did above.    <br>
--- we'll merge the original data frame with the data frame containing  <br>
--- average steps by interval to form a third, new data frame. We'll   <br>
--- impute values for the NA's in the new data frame.  <br>

--- 'join' the two data frames using merge()  <br>
activity_imputed <- merge(activity, average_steps_by_time, by = "interval")  <br>
---  correct the NA steps with average steps for the interval  <br>

activity_imputed <- within(activity_imputed, steps <- ifelse(is.na(activity_imputed$steps), activity_imputed$average_steps, activity_imputed$steps))


--- Now calculate the total number of steps per day with the imputed values.  <br>

total_steps_by_date_imputed <- aggregate(list(total_steps = activity_imputed$steps),  by = list(date = activity_imputed$date), FUN = sum, na.rm = FALSE)  <br>


--- Draw histograms showing the distribution of total steps <br>
--- (frequency and density) with the imputed values.  <br>

par(mfrow = c(1, 2))

--- frequencies
png("plot4.png")

hist(total_steps_by_date_imputed$total_steps, breaks = 30, xlab = "Total Steps", main = "Total Steps Per Day", col = "lightblue")

dev.off()
--- desnsity

png("plot5.png")

plot(density(total_steps_by_date_imputed$total_steps, na.rm = TRUE), xlab = "Total Steps",  ylab = "Density", main = "Total Steps Per Day", col = "purple", lwd = 3)

dev.off()

par(mfrow = c(1, 1))

--- Finally, we'll calculate the mean and median number of steps per day. <br>

mean(total_steps_by_date_imputed$total_steps)  <br>
--- [1] 10766.19  <br>

median(total_steps_by_date_imputed$total_steps)  <br>
--- [1] 10766.19  <br>

--- The mean and median total number of steps are now equal to one  <br>
--- another (!!!) and higher with the imputed values. Estimates of the   <br>
--- total daily number of steps are higher with the imputed values.  <br>


# [5] Are there differences in activity patterns between 
#     weekdays and weekends?
<br>
<br>
--- Add a factor called weekend_indicator with two levels to the <br>
--- data set indicating whether the date is a weekday or a weekend. <br>

--- first add a character column for day of the week  <br>
<br>
activity_imputed$weekday <- weekdays(activity_imputed$date)  <br>

--- now populate a new factor column using day of the week and a simple <br>
--- function  <br>
<br>

activity_imputed$weekend_indicator <- as.factor(apply(activity_imputed["weekday"],  1, function(x) {
        switch(x, Sunday = "weekend", Saturday = "weekend", "weekday")
    }))   <br>

<br>
 
--- confirm that we have the character and factor types we expect  <br>

str(activity_imputed)  <br>
--- 'data.frame':    17568 obs. of  8 variables:  <br>
---  $ interval         : int  0 0 0 0 0 0 0 0 0 0 ...  <br>
---  $ steps            : num  1.72 0 0 0 0 ...  <br>
---  $ date             : POSIXct, format: "2012-10-01" "2012-11-23" ...<br>
---  $ time.x           : POSIXct, format: "2014-11-22 00:00:00" "2014-11-22 00:00:00" ... <br>
---  $ time.y           : POSIXct, format: "2014-11-22 00:00:00" "2014-11-22 00:00:00" ... <br>
---  $ average_steps    : num  1.72 1.72 1.72 1.72 1.72 ... <br>
<br>
<br>
--- Now draw a panel plot using ggplot2, comparing activity patterns on <br>
--- weekdays and weekends.   <br>

average_steps_by_time_weekend <- aggregate(list(average_steps = activity_imputed$steps), 
by = list(time = activity_imputed$time.x, daytype = activity_imputed$weekend_indicator), FUN = mean )


library(ggplot2)

png("plot6.png")

qplot(x = time, y = average_steps, geom = "path", data = average_steps_by_time_weekend, 
xlab = "Time interval", ylab = "Average steps", 
main = "Activity Patterns\nWeekdays vs. Weekends", 
    facets = daytype ~ .)
	
dev.off()


