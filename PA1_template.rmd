library(ggplot2)
library(dplyr)
activity<-read.csv("activity.csv")
str(activity)
stepsperday <- aggregate(activity$steps, list(activity$date), FUN=sum)
colnames(stepsperday) <- c("Date", "Steps")
stepsperday
hist(stepsperday$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day")
#mean
as.integer(mean(stepsperday$Steps,na.rm=TRUE))

#median
as.integer(median(stepsperday$Steps,na.rm=TRUE))

stepspertime <- aggregate(steps~interval,data=activity,FUN=mean,na.action=na.omit)
stepspertime$time <- stepspertime$interval/100
h <- ggplot(stepspertime, aes(time, steps))
h+geom_line(col="red")+ggtitle("Average steps per time interval")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))
ST<-tbl_df(stepspertime)
ST %>% select(time,steps) %>% filter(steps==max(ST$steps))

#Total number of missing values
ACT <- tbl_df(activity)
ACT %>% filter(is.na(steps)) %>% summarize(missing_values = n())

#Missing Values
activity$CompleteSteps <- ifelse(is.na(activity$steps), round(stepspertime$steps[match(activity$interval, stepspertime$interval)],0), activity$steps)
activity$CompleteSteps

#New dataset activityFull
activityFull <- data.frame(steps=activity$CompleteSteps, interval=activity$interval, date=activity$date)
activityFull

# prepare data
StepsPerDayFull <- aggregate(activityFull$steps, list(activityFull$date), FUN=sum)
colnames(StepsPerDayFull) <- c("Date", "Steps")
# draw the histogram
g <- ggplot(StepsPerDayFull, aes(Steps))
g+geom_histogram(boundary=0, binwidth=2500, col="darkblue", fill="lightblue")+ggtitle("Histogram of steps per day")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))+scale_x_continuous(breaks=seq(0,25000,2500))+scale_y_continuous(breaks=seq(0,26,2))

# Mean
mean(StepsPerDayFull$Steps)

#Median
median(StepsPerDayFull$Steps)

# Create variable with date in correct format
activityFull$RealDate <- as.Date(activityFull$date, format = "%Y-%m-%d")
# create a variable with weekdays name
activityFull$weekday <- weekdays(activityFull$RealDate)
# create a new variable indicating weekday or weekend
activityFull$DayType <- ifelse(activityFull$weekday=='Saturday' | activityFull$weekday=='Sunday', 'weekend','weekday')
# see first 10 values
head(activityFull, n=10)

# create table with steps per time across weekdaydays or weekend days
StepsPerTimeDT <- aggregate(steps~interval+DayType,data=activityFull,FUN=mean,na.action=na.omit)
# variable time (more comprensible for the graph axis)
StepsPerTimeDT$time <- stepspertime$interval/100
# draw the line plot
j <- ggplot(StepsPerTimeDT, aes(time, steps))
j+geom_line(col="darkred")+ggtitle("Average steps per time interval: weekdays vs. weekends")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))+facet_grid(DayType ~ .)
