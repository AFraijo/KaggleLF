##New Try, years later
##Author: Me

library(tidyverse)
library(lubridate)
library(feather)

##load("Load_Forecasting.Rproj.RData")
##bad habit, but let's save the data incase we mess up
write_feather(Load_history, "Load_history.feather") 
write_feather(temperature_history, "temperature_history.feather")

##this is not in a tidy format which makes some things difficult
##change date format

create_date <- function(YEAR, MONTH, DAY){
  date <- paste(YEAR, MONTH, DAY, sep="-") %>% ymd()
  return(date)
}

Load_history <- Load_history %>% 
  mutate(Date = create_date(year, month, day)) %>%
  mutate_at(vars(starts_with("h")), funs(as.character(.))) %>%
  mutate_at(vars(starts_with("h")), funs(gsub(",", "", .))) %>%
  mutate_at(vars(starts_with("h")), funs(as.numeric(.)))
##funtions to create time stamps
strip_time <- function(STRING){
  hour1 <- substr(STRING,2,2)
  hour2 <- as.character(as.numeric(hour1))
  time <- paste(hour2,'00',sep=':')
  return(time)
}

create_time_stamp <- function(DATE, HOUR){
  time_stamp <- paste(DATE, HOUR, sep=" ") %>% ymd_hm()
  return(time_stamp)
}

##Let's tidy this ****
Load_history_Tidy <- Load_history %>%  
  gather(key = "time", value = "kWh", starts_with("h")) %>% 
  mutate(Hour = strip_time(time),
         Time_stamp = create_time_stamp(Date, Hour)) %>% 
  select(-year, -month, -day, -Date, - time, -Hour)

#### Now for the Temperature data
temperature_history <- temperature_history %>% 
  mutate(Date = create_date(year, month, day))

Temp_history_Tidy <- temperature_history %>%  
  gather(key = "time", value = "Temp", starts_with("h")) %>% 
  mutate(Hour = strip_time(time),
         Time_stamp = create_time_stamp(Date, Hour)) %>% 
  select(-year, -month, -day, -Date, - time, -Hour)

write_feather(Load_history_Tidy,"Load_tidy.feather")
write_feather(Temp_history_Tidy,"Temp_tidy.feather")


##The temperature stations do not correspond to load zones, so
##we will spread the data out, add an average value and then join with the
##load data. This should let us use the data for modeling.

All_data_tidy <- left_join(Load_history_Tidy, Temp_history_Tidy, by = )

