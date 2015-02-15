# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("activity.zip" ,"activity.csv")
activ <- read.csv(file = "activity.csv")
```


## What is mean total number of steps taken per day?

```r
totals <- aggregate(steps ~ date, data=activ, sum)

hist(totals$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
meanSteps <- mean(totals$steps)
meanSteps
```

```
## [1] 10766.19
```

```r
medianSteps <- median(totals$steps)
medianSteps
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
avs <- aggregate(steps ~ interval, data=activ, mean)
plot(avs, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
maxSteps <- max(avs$steps)
maxSteps
```

```
## [1] 206.1698
```

```r
subset(avs, steps == max(steps), select = c(interval, steps))
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

```r
sum(is.na(activ))
```

```
## [1] 2304
```

```r
df = transform(activ, steps = ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))

df.sum <- aggregate(steps ~ date, data=df, FUN = sum)

hist(df.sum$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
df.meanSteps <- mean(df.sum$steps)
df.meanSteps
```

```
## [1] 10766.19
```

```r
df.medianSteps <- median(df.sum$steps)
df.medianSteps
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

```r
test <- df
test$dow <- ifelse(weekdays(as.Date(test$date)) %in% c("Saturday", "Sunday"), yes = "weekend", no = "weekday")
test$dow <- as.factor(test$dow)
weekdays <- subset(test, dow == "weekday")
wkdays.ag <- aggregate(steps ~ interval, data=weekdays, FUN=mean)
weekends <- subset(test, dow == "weekend")
wkends.ag <- aggregate(steps ~ interval, data=weekends, FUN=mean)
par(mfrow=c(2,1))
plot(wkdays.ag$interval, wkdays.ag$steps, main="Weekdays", type="l")
plot(wkends.ag$interval, wkends.ag$steps, main="Weekends", type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
