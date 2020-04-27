
library(ggplot2)



data <- read.csv("activity.csv")



totalSteps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(totalSteps, binwidth=1000, xlab="total number of steps taken each day")
mean(totalSteps, na.rm=TRUE)
median(totalSteps, na.rm=TRUE)


average <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=average, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken")


average[which.max(average$steps),]


missing <- is.na(data$steps)
table(missing)
filled.value<- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (average[average$interval==interval, "steps"])
  return(filled)
}
filled.data <- data
filled.data$steps <- mapply(filled.value , filled.data$steps, filled.data$interval)



total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps)
median(total.steps)


weekdayweekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekdayweekend)

average <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(average, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")

