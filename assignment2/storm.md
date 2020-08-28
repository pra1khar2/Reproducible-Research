Storms and other severe weather events cause both public health and economic.
========================================================
Shenda Hong
# Synopsis
Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

The basic goal of this assignment is to explore the NOAA Storm Database and answer some basic questions about severe weather events. You must use the database to answer the questions below and show the code for your entire analysis. Your analysis can consist of tables, figures, or other summaries. You may use any R package you want to support your analysis.

# Data Processing
## Reading data and modifying names

```r
# Loading Data
setwd("G:\\Learn\\Coursera\\Data Science\\5Reproducible Research\\assignment2")
data <- read.csv("StormData.csv", sep = ",", na.strings = c("NA", " "), stringsAsFactors = F)
names(data)
```

```
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"    
##  [6] "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN"
## [16] "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"   
## [26] "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
## [36] "REMARKS"    "REFNUM"
```

```r
# Modifying variable names
nameList <- names(data)
nameList <- tolower(make.names(nameList, allow_ = F))
colnames(data) <- nameList
names(data)
```

```
##  [1] "state.."    "bgn.date"   "bgn.time"   "time.zone"  "county"    
##  [6] "countyname" "state"      "evtype"     "bgn.range"  "bgn.azi"   
## [11] "bgn.locati" "end.date"   "end.time"   "county.end" "countyendn"
## [16] "end.range"  "end.azi"    "end.locati" "length"     "width"     
## [21] "f"          "mag"        "fatalities" "injuries"   "propdmg"   
## [26] "propdmgexp" "cropdmg"    "cropdmgexp" "wfo"        "stateoffic"
## [31] "zonenames"  "latitude"   "longitude"  "latitude.e" "longitude."
## [36] "remarks"    "refnum"
```

## Subseting useful informations

```r
# Subseting useful informations
pop <- subset(data, fatalities > 0 | injuries > 0, c("bgn.date", "evtype", "fatalities", 
    "injuries"))
# Translating variable into proper types
pop$evtype <- as.factor(toupper(pop$evtype))
pop$fatalities <- as.numeric(pop$fatalities)
```

```
## Warning: NAs introduced by coercion
```

```r
pop$injuries <- as.numeric(pop$injuries)
```

```
## Warning: NAs introduced by coercion
```


```r
pop <- subset(pop, fatalities > 0 | injuries > 0, c("bgn.date", "evtype", "fatalities", 
    "injuries"))
names(pop)
```

```
## [1] "bgn.date"   "evtype"     "fatalities" "injuries"
```

```r
summary(pop)
```

```
##    bgn.date                    evtype       fatalities       injuries     
##  Length:16731       TORNADO       :6994   Min.   :  0.0   Min.   :   0.0  
##  Class :character   TSTM WIND     :2675   1st Qu.:  0.0   1st Qu.:   1.0  
##  Mode  :character   LIGHTNING     :2304   Median :  0.0   Median :   1.0  
##                     FLASH FLOOD   : 575   Mean   :  0.7   Mean   :   7.2  
##                     EXCESSIVE HEAT: 499   3rd Qu.:  1.0   3rd Qu.:   4.0  
##                     HIGH WIND     : 379   Max.   :583.0   Max.   :1700.0  
##                     (Other)       :3305   NA's   :1       NA's   :6
```

```r

# Subseting useful informations
eco <- subset(data, propdmg > 0 | cropdmg > 0, c("bgn.date", "evtype", "propdmg", 
    "propdmgexp", "cropdmg", "cropdmgexp"))
```


```r
# Translating variable into proper types
eco$evtype <- as.factor(toupper(eco$evtype))
eco$propdmg <- as.numeric(eco$propdmg)
```

```
## Warning: NAs introduced by coercion
```

```r
eco$cropdmg <- as.numeric(eco$cropdmg)
```

```
## Warning: NAs introduced by coercion
```

```r
eco$propdmgexp <- with(eco, ifelse(propdmgexp %in% c("K", "k", "M", "m", "B"), 
    propdmgexp, 0))
eco$cropdmgexp <- with(eco, ifelse(cropdmgexp %in% c("K", "k", "M", "m", "B"), 
    cropdmgexp, 0))

eco <- subset(eco, propdmg > 0 | cropdmg > 0, c("bgn.date", "evtype", "propdmg", 
    "propdmgexp", "cropdmg", "cropdmgexp"))
names(eco)
```

```
## [1] "bgn.date"   "evtype"     "propdmg"    "propdmgexp" "cropdmg"   
## [6] "cropdmgexp"
```

```r
summary(eco)
```

```
##    bgn.date                        evtype         propdmg     
##  Length:140771      TSTM WIND         :48937   Min.   :  0.0  
##  Class :character   TORNADO           :33077   1st Qu.:  2.5  
##  Mode  :character   HAIL              :15552   Median :  8.0  
##                     THUNDERSTORM WINDS:12005   Mean   : 47.7  
##                     FLASH FLOOD       :10399   3rd Qu.: 26.0  
##                     LIGHTNING         : 5895   Max.   :990.0  
##                     (Other)           :14906   NA's   :2      
##   propdmgexp           cropdmg       cropdmgexp       
##  Length:140771      Min.   :  0.0   Length:140771     
##  Class :character   1st Qu.:  0.0   Class :character  
##  Mode  :character   Median :  0.0   Mode  :character  
##                     Mean   :  5.4                     
##                     3rd Qu.:  0.0                     
##                     Max.   :990.0                     
##                     NA's   :2
```

## Calculating actual dmg with dmgexp

```r
# function return multiplier
mul <- function(x) {
    if (x == "K" | x == "k") {
        y <- 1/1000
    } else if (x == "B" | x == "b") {
        y <- 1000
    } else {
        y <- 1
    }
    y
}
# Calculating actual dmg with dmgexp
eco$propmul <- sapply(eco$propdmgexp, mul)
eco$cropmul <- sapply(eco$cropdmgexp, mul)
eco$propdmg1 <- eco$propdmg * eco$propmul
eco$cropdmg1 <- eco$cropdmg * eco$cropmul
```


# Results
## Across the United States, which types of events are most harmful with respect to population health?

```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.0.2
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.0.2
```

```r
# Summing fatalities and injuries by evtype
popSum <- ddply(pop, .(evtype), summarise, fatality.sum = sum(fatalities), injury.sum = sum(injuries))
popSum$dmg <- popSum$fatality.sum + popSum$injury.sum
popTop10 <- popSum[order(popSum$dmg, decreasing = T)[1:10], ]
```

The rank of events which are the most top 10 harmful with respect to population health is:

```r
print(popTop10)
```

```
##             evtype fatality.sum injury.sum   dmg
## 170        TORNADO         4658      80084 84742
## 177      TSTM WIND          471       6452  6923
## 46           FLOOD          258       6499  6757
## 32  EXCESSIVE HEAT         1416       4354  5770
## 116      LIGHTNING          562       3628  4190
## 110      ICE STORM           76       1959  2035
## 41     FLASH FLOOD          559       1407  1966
## 66            HEAT          708        878  1586
## 199   WINTER STORM          170       1238  1408
## 64            HAIL           12       1154  1166
```


```r
# Plot result
qplot(reorder(evtype, -dmg), dmg, evtype, data = popTop10, geom = "bar", stat = "identity", 
    main = "The most harmful event with respect to population health", xlab = "Types of events", 
    ylab = "Damagement")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

## Across the United States, which types of events have the greatest economic consequences?

```r
# Summing propdmg and cropdmg by evtype
ecoSum <- ddply(eco, .(evtype), summarise, prop.sum = sum(propdmg1), crop.sum = sum(cropdmg1))
ecoSum$dmg <- ecoSum$prop.sum + ecoSum$crop.sum
ecoTop10 <- ecoSum[order(ecoSum$dmg, decreasing = T)[1:10], ]
print(ecoTop10)
```

```
##                 evtype prop.sum crop.sum   dmg
## 317            TORNADO  41276.4    376.0 41652
## 180  HURRICANE/TYPHOON  19403.4    586.8 19990
## 67               FLOOD  12345.1   2333.9 14679
## 172          HURRICANE   9400.7   2561.4 11962
## 36             DROUGHT    845.3   9860.2 10706
## 232        RIVER FLOOD   5118.9   5029.5 10148
## 101               HAIL   7950.2   1968.8  9919
## 54         FLASH FLOOD   8658.1    642.8  9301
## 292 THUNDERSTORM WINDS   7909.2    278.7  8188
## 189          ICE STORM   3003.1   5022.0  8025
```

The rank of events which are the most top 10 harmful with respect to population health is:

```r
print(popTop10)
```

```
##             evtype fatality.sum injury.sum   dmg
## 170        TORNADO         4658      80084 84742
## 177      TSTM WIND          471       6452  6923
## 46           FLOOD          258       6499  6757
## 32  EXCESSIVE HEAT         1416       4354  5770
## 116      LIGHTNING          562       3628  4190
## 110      ICE STORM           76       1959  2035
## 41     FLASH FLOOD          559       1407  1966
## 66            HEAT          708        878  1586
## 199   WINTER STORM          170       1238  1408
## 64            HAIL           12       1154  1166
```


```r
# Plot result
qplot(reorder(evtype, -dmg), dmg, evtype, data = ecoTop10, geom = "bar", stat = "identity", 
    main = "The most harmful event with respect to population health", xlab = "Types of events", 
    ylab = "Damagement")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 



