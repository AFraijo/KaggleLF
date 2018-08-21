##Opening EDA work
##Author: Me

##Strategy: Let's start by graphing the timeseries at various levels
##Weekdays
##Months
##Hour of day

library(tidyverse)
library(tsibble)
library(lubridate)
library(sugrrants)

options(scipen = 100000000)
##library(feather)
##read_feather("All_data_tidy.feather")

##Ok, so after all that we actually need to add hour, month, and weekday back in
##so that we can roll up to those
##Actually, could I have use the tsibble index_by variabe?

All_data_tidy <- All_data_tidy %>% 
  mutate(Month = month(Time_stamp, label = TRUE, abbr = FALSE),
         Day = wday(Time_stamp, label = TRUE, abbr = FALSE),
         Hour = hour(Time_stamp))

##All_data_tidy <- All_data_tidy %>% 
##  mutate(Month = month(Time_stamp, label = TRUE, abbr = FALSE),
##         Day = wday(Time_stamp, label = TRUE, abbr = FALSE),
##         Hour = hour(Time_stamp))




All_data_tsbl <- as_tsibble(All_data_tidy, key = id(zone_id), index = Time_stamp)

All_data_tsbl %>% group_by(zone_id) %>% 
  index_by(Month = yearmonth(Time_stamp)) %>% 
  summarize(Ave_kWh = mean(kWh, na.rm = TRUE)) %>% 
  ggplot(aes(x = Month, y = Ave_kWh)) + geom_line() +
  facet_wrap(~zone_id, ncol = 5) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90))
  facet_wrap(~zone_id, ncol = 5)

##This is ok, but wonky. Let's try a regular dplyr approach
All_data_tidy %>% group_by(zone_id, Month) %>% 
  summarize(Ave_kWh = mean(kWh, na.rm = TRUE)) %>% 
  ggplot(aes(x = Month, y = Ave_kWh)) + geom_line(aes(group = zone_id)) +
  facet_wrap(~zone_id, ncol = 5) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90))

##something is very odd with 4, but also 5, 8, 9
All_data_tsbl %>% filter(zone_id == 4) %>% 
  index_by(Month = yearmonth(Time_stamp)) %>% 
  summarize(Ave_kWh = mean(kWh, na.rm = TRUE)) %>% 
  ggplot(aes(x = Month, y = Ave_kWh)) + geom_line() + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90))
##Maybe this is not odd. Maybe this zone is just a low volume zone

##Should I look at quater? I think we aleady see the seasonality, so that might 
##not help

###Let's look at weekday
All_data_tidy %>% group_by(zone_id, Day) %>% 
  summarize(Ave_kWh = mean(kWh, na.rm = TRUE)) %>% 
  ggplot(aes(x = Day, y = Ave_kWh)) + geom_line(aes(group = zone_id)) +
  facet_wrap(~zone_id, ncol = 5) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90))
##this is much more interesting. 3,5,6,7,10,11,17,20 show what you would expect
##with the peaks in the middle

##Hour
All_data_tidy %>% group_by(zone_id, Hour) %>% 
  summarize(Ave_kWh = mean(kWh, na.rm = TRUE)) %>% 
  ggplot(aes(x = Hour, y = Ave_kWh)) + geom_line(aes(group = zone_id)) +
  facet_wrap(~zone_id, ncol = 5) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90))
##Also interesting