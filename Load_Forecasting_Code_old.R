## Author: Andrew Fraijo
## This is the code for an attempt at the Kaggle Load Forecasting from 2012

library(dplyr)
library(lubridate)
library(magrittr)
library(reshape2)

#load data files
Load_history <- read.csv("~/Documents/Projects/Load History Data/Load_history.csv")
temperature_history <- read.csv("~/Documents/Projects/Load History Data/temperature_history.csv")

#Functions list
#Functions for creating time stamp
create_date <- function(YEAR, MONTH, DAY){
  date <- paste(YEAR, MONTH, DAY, sep="-") %>% ymd()
  return(date)
}

create_time_stamp <- function(DATE, HOUR){
  time_stamp <- paste(DATE, HOUR, sep=" ") %>% ymd_hm()
  return(time_stamp)
}

#this might be better if there were a dict structure I could use to map
#from one to another
strip_time <- function(STRING){
  hour1 <- substr(STRING,2,2)
  hour2 <- as.character(as.numeric(hour1)-1)
  time <- paste(hour2,'30',sep=':')
  return(time)
}

clean_frame <-function(ID){
  station <- filter(temperature_history, station_id==ID)
  station <- mutate(station, date = create_date(year, month, day))
  station <- select(station, -station_id, -year, -month, -day)
  station_ts <- melt(station, id = "date", variable.name = "hour", value.name ="temp")
  station_ts <- mutate(station_ts, hour = strip_time(hour))
  station_ts <- mutate(station_ts, time = create_time_stamp(date, hour))
  station_ts <- station_ts %>% select(time, temp)
}