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

##library(feather)
##read_feather("All_data_tidy.feather")

##Ok, so after all that we actually need to add hour, month, and weekday back in
##so that we can roll up to those
##Actually, could I have use the tsibble index_by variabe?
##All_data_tidy <- All_data_tidy %>% 
##  mutate(Month = month(Time_stamp, label = TRUE, abbr = FALSE),
##         Day = wday(Time_stamp, label = TRUE, abbr = FALSE),
##         Hour = hour(Time_stamp))



All_data_tsbl <- as_tsibble(All_data_tidy, key = id(zone_id), index = Time_stamp)

All_data_tsbl %>% group_by(zone_id) %>% 
  index_by(Month = yearmonth(Time_stamp)) %>% 
  summarize(Ave_kWh = mean(kWh, na.rm = TRUE)) %>% 
  ggplot(aes(x = Month, y = Ave_kWh)) + geom_col() +
  facet_wrap(~zone_id, ncol = 5)
##This is ok, but wonky. Let's try a regular dplyr approach
All_data_tidy %>% group_by(zone_id, Month) %>% 
  summarize(Ave_kWh = mean(kWh, na.rm = TRUE)) %>% 
  ggplot(aes(x = Month, y = Ave_kWh)) + geom_col() +
  facet_wrap(~zone_id, ncol = 5)


