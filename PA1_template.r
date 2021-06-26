#PA1.template.R

#Libraries

options(stringsAsFactors = FALSE )        # no automatic data transformation
options(scipen = 999, digits = 15)        #suppress scientific notation
#options(na.action = na.warn)
#options(na.action = na.exclude)
library(readr,quietly=TRUE)               #needed for read_csv
library(ggplot2,quietly=TRUE)             #used to make histogram
library(dplyr,quietly=TRUE)               #needed for %>% etc
library(tidyr, quietly = TRUE)
require(gridExtra, quietly= TRUE) 

## 2 - Loading and preprocessing the data  
abb <- getwd()    #the repdata_data_activity.zip and the activity.cs files are in this wrkdir

if(!file.exists("activity.csv")){	   
  unzip(zipfile="repdata_data_activity.zip")}

#### 2.1a - Process the data (if necessary) into a format suitable for the analysis 

datap <- read_csv("activity.csv", na="NA", col_names = TRUE,
                  col_types = cols(steps = col_integer(),
                                   date = col_date(format = "%Y-%m-%d"),
                                   interval = col_integer()),
                  locale = default_locale())

str(datap)
summary(datap)

### 3.1 - The mean total number of steps taken per day

#### 3.1.1 - Calculate the total number of steps taken per day

totalNsteps <- datap %>%
  group_by(date) %>%
  summarise(stepsEachDay = sum(steps, na.rm = TRUE)) 

#### 3.1.2 - Histogram of the total number of steps taken each day
hist(totalNsteps$stepsEachDay, xlab= "Number of steps", main="Histogram: Steps per Day")

#### 3.1.3 - The mean and median of the total number of steps taken each day
mean_median <- summarise(totalNsteps, 
                         Mean   = mean(stepsEachDay, na.rm = TRUE), 
                         Median = median(stepsEachDay, na.rm = TRUE))

#The mean and median of the total number of steps taken each day are 

Mean      = mean_median[, 1]      
Median    = mean_median[, 2]

## 4 - The average daily activity pattern

### 4.1.	Make a time series plot (type = "l") of the 5-minute interval (x-axis)   
#and the average number of steps taken, averaged across all days (y-axis)
steps_by_interval <- datap %>%
  group_by(interval) %>%
  summarise(average_steps = mean(steps, na.rm = TRUE))

g <- ggplot(data = steps_by_interval, aes(x = interval, y = average_steps)) +
  geom_line() +
  xlab("5-minute Interval Identifier") + 
  ylab("average number of steps") 

plot(g)
rm(g)

dev.off()

### 4.2 The 5-minute interval, on average across all the days in the dataset,
#that contains the maximum number of steps.

Maxsteps_in5min <- steps_by_interval[steps_by_interval$average_steps == 
                                       max(steps_by_interval$average_steps), ]

#The five minute interval on average across all the days in the dataset that contains 
#the maximum number of steps is 

head(Maxsteps_in5min[1])

## 5 - Imputing Missing Values   

### 5.1.	Calculate and report the total number of missing values in the dataset 

totalmissing <-  sum(is.na(datap$steps))

#The total number of NAs in each column of the dataset ***datap***

nmissing_colwise <- colSums(is.na(datap))
head(nmissing_colwise)

### 5.2 Strategy for filling in all of the missing values in the dataset. 

#for each interval in the dataset (recall that the intervals are periodic in 
#that they repeat for each day). Each step which has a mssing value is assigned
#the mean (or the median)that corresponds 
#to the interval that the particular step with missing value is associated.

#### 5.3.1  Creating secondary dependent fields for ease of use   

datap <- datap %>% mutate(min_day  = as.integer(as.integer(datap$interval+5)- 
                                                  as.integer(datap$interval/100)*40)) 

datap <- datap %>% mutate (hr_day  = as.integer(ifelse((datap$min_day%% 60) == 0, 
                                                       datap$min_day%/%60,                                                                     (datap$min_day%/%60) +1)),
                           day_week = as.integer(strftime(datap$date, format = "%u")),                           
                           miss_val   =ifelse(is.na(steps), TRUE, FALSE))

# **min_day** is a five minute interval in a 24 hour day. 
#There are 288 such intervals in each 24 hour day referenced sequentially 
#from 1 thru 288. The cycle repeats for the next 24 hour day. 

#hr_day is the hour in a 24 hour day numbered sequentially from 1 thru 24. 
#Each hr_day has 288 min_day. 
#day_week  is the day of the week beginning Mon as day 1 and Sun as day 7. 
#The logical variable **miss_val** is TRUE if the steps is a missing value (NA) and FALSE if steps hav a value >= 0. 

#Below, abbreviated name of the weekday **wkdname** and a field **wkdayid** 
#that takes the value **weekDay** if **wkdname** is one of Mon, Tue, 
#Wed, Thu, or Fri or the value **weekEnd** if **wkdname** is Sat or Sun.

datap <- datap %>% mutate (wkdname  = (strftime(datap$date, format = "%a")),
                           rnum = row_number())
datap <- datap %>% group_by(wkdname) %>% 
  mutate (wkdayid = as.character(ifelse((wkdname== "Sat" | wkdname == "Sun"), 
                                        'WeekEnd', 'WeekDay')))

#To Impute missing values we add two columns to datap - the mean_stpm (mean of steps)
# and median_stpm, the median of the steps

datap <- datap %>% group_by(min_day) %>% mutate(
  mean_stpm =  mean(steps,na.rm=TRUE),
  median_stpm = median(steps, na.rm=TRUE)) %>%
  ungroup()

### 5.4 A new dataset equal to the original dataset (**datap**) with 
#the missing data filled in

satap <- datap %>% transmute(steps = ifelse(miss_val, mean_stpm, steps))  %>% 
  ungroup()

datap$steps <- satap$steps
rm(satap)

#### 5.5.1 - Calculate the total number of steps taken per day
totalNstepsf <- datap %>%
  group_by(date) %>%
  summarise(stepsEachDayf = sum(steps, na.rm = TRUE)) 

#### 5.5.2 - Histogram of the total number of steps taken each day
hist(totalNstepsf$stepsEachDayf, xlab= "Number of steps", main="Histogram: Steps per Day")

#### 5.5.3 - The mean and median of the total number of steps taken each day

mean_median2 <- totalNstepsf %>% summarise(
  Mean_imp   = mean(stepsEachDayf, na.rm = TRUE), 
  Median_imp = median(stepsEachDayf, na.rm = TRUE))

summary(mean_median2)

#### 5.5.4 - Do these values of mean and median differ from the estimates from the first 
#### part of the assignment?

Mean_difference =     mean_median2[, 1] - mean_median[, 1]   #steps and;     
Median_difference =   mean_median2[, 2] - mean_median[, 2]   #steps   

## 6 - Are there differences in activity patterns between weekdays and weekends?
sc <- datap %>% group_by(wkdayid,min_day) %>% 
  summarize(wkavgsteps = mean(steps, na.rm=TRUE))

grpwkday <- sc[sc$wkdayid== "WeekDay", ]
grpwkend <- sc[sc$wkdayid== "WeekEnd", ]

wdx <- ggplot(grpwkday, aes(x=min_day, y=wkavgsteps)) +
  geom_line(color="firebrick") +
  labs(x = "5-minute Intervals") +
  labs(y = "Average steps taken/5 ") +
  labs(caption = "Weekday Activities") +
  theme(plot.caption = element_text(hjust = 0.5, size=10)) 

wex <- ggplot(grpwkend, aes(x=min_day, y=wkavgsteps)) +
  geom_line(color= "#003153") +
  labs(x = "5-minute Intervals") +
  labs(y = "Average steps taken/5 ") +
  labs(caption = "Weekend Activities") +
  theme(plot.caption = element_text(hjust = 0.5, size=10))

grid.arrange(wdx,wex, nrow=1)   #using library gridExtra

dev.off()

#source("PA1_template.r")
