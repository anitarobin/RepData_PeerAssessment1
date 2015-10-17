# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
#read the data
        df<-read.csv(".\\activity\\activity.csv")
#omit the na values
        df_trunc<-na.omit(df)
```

## What is mean total number of steps taken per day?

```r
#loading required library
        library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
#grouping by date
        dates<-group_by(df_trunc,date)
#summarizing for calculating total number of steps for each day
        dfSums<-summarise(dates,sum=sum(steps))

#calculating mean of total number of steps
      m<-as.character(round(mean(dfSums$sum),digits = 0))

#calculating median total number of steps
      md<-as.character(round(median(dfSums$sum),digits=0))
```

## *Mean total number of steps is 10766 and median value is 10765.*


![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

## What is the average daily activity pattern?


```r
#setting up interval as factor
        df$interval<-factor(df$interval,levels=unique(df$interval),
                labels=as.character(unique(df$interval)))
#grouping by intervals
        intervals=group_by(df_trunc,interval)
#finding the means
        dfMeans<-summarise(intervals,mean=mean(steps))
#setting up the plot
        plot(dfMeans, type = "l", xlab = "intervals", main = "Average Daily Activity pattern", ylab = "Average number of steps")
        points(dfMeans, pch = 20, col = "Red")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
#finding the interval that has the max no. of steps
        Int_max<-as.character(dfMeans[which(dfMeans[,'mean'] == max(dfMeans$mean)),1 ])
```


## *The interval  835 contains the max number of steps on average across all days.*
## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
