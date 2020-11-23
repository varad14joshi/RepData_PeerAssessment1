---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 4.0.3
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.0.3
```

```r
data<-read.csv("activity.csv")
```





## What is mean total number of steps taken per day?

```r
j<-data %>% group_by(date) %>% summarise(no_steps=sum(steps,na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
hist(j$no_steps,col="blue",xlab="No of Steps",ylab = "Average no of Steps",main="Total no of steps per day ",ylim = c(0,35))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
print(paste("Median of total number of steps per day",median(j$no_steps)))
```

```
## [1] "Median of total number of steps per day 10395"
```

```r
print(paste("Mean of total number of steps per day",mean(j$no_steps)))
```

```
## [1] "Mean of total number of steps per day 9354.22950819672"
```



## What is the average daily activity pattern?


```r
j1<-data %>% group_by(interval) %>% summarise(no_steps=mean(steps,na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
plot(j1$interval,j1$no_steps,type = 'l',col="blue",xlab="Interval",ylab = "Average no of Steps",main="Average no of steps over intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
m<-j1$interval[which.max(j1$no_steps)]

print(paste("The 5 minute interval which contains the maximum number of steps is",m))
```

```
## [1] "The 5 minute interval which contains the maximum number of steps is 835"
```




## Imputing missing values

```r
final<-data
for( i in  1:nrow(final))
{
  if (is.na(final[i,1])==TRUE)
  {
    final[i,1]<-j1[which(final[i,3]==j1[,1]),2]
  }
}

j2<-final %>% group_by(date) %>% summarise(no_steps=sum(steps,na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
hist(j$no_steps,col="blue",xlab="No of Steps",ylab = "Average no of Steps",main="Total no of steps per day ",ylim = c(0,35))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
namean<-data.frame(mean=c(mean(j$no_steps),mean(j2$no_steps)),median=c(median(j$no_steps),median(j2$no_steps)))

row.names(namean)<-c("With Missing Values","Without Missing values")

namean
```

```
##                            mean   median
## With Missing Values     9354.23 10395.00
## Without Missing values 10766.19 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

```r
final[,2]<-as.Date(final[,2])

final<-mutate(final,days= weekdays(final$date))

for( i in 1:nrow(final))
{
  if ( final[i,4] %in% c("Saturday","Sunday"))
  {
    final[i,4]<-"Weekend"
  }else
  {
    final[i,4]<-"Weekday"
  }
}

j3<-final %>% group_by(interval,days) %>% summarise(no_steps=mean(steps,na.rm = TRUE))
```

```
## `summarise()` regrouping output by 'interval' (override with `.groups` argument)
```

```r
g<-ggplot(j3,aes(interval,no_steps))
g+geom_line()+facet_grid(days~.)+ggtitle("Average Number of Steps - Weekday vs. Weekend") 
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


