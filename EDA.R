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

##Copying idea from Rob Hyndman et. al
##http://www.nxtbook.com/nxtbooks/pes/powerenergy_050618/index.php#/22

Monthly_quantiles <- All_data_tidy %>% filter(!is.na(kWh)) %>% 
  select(zone_id, Month, kWh) %>% group_by(zone_id, Month) %>% 
  summarize(`10` = quantile(kWh, probs = 0.1),
            `25` = quantile(kWh, probs = 0.25),
            `50` = quantile(kWh, probs = 0.5),
            `75` = quantile(kWh, probs = 0.75),
            `90` = quantile(kWh, probs = 0.9)) 

##add jitter and labels
cols <- c("10" = "#E69F00", "25" = "#56B4E9", "50" = "#F0E442", "75" = "#009E73", "90" = "#CC79A7")

Monthly_Scatter <- All_data_tidy %>% filter(!is.na(kWh)) %>% 
  select(zone_id, Month, kWh) %>% 
  ggplot(aes(x = Month, y = kWh)) + geom_point(aes(group = zone_id)) + geom_jitter(size = .1, alpha = 0.1) + 
    geom_line(data = Monthly_quantiles, aes(x = Month, y = `10`, group = zone_id, colour = "10")) +
    geom_line(data = Monthly_quantiles, aes(x = Month, y = `25`, group = zone_id, colour = "25")) +
    geom_line(data = Monthly_quantiles, aes(x = Month, y = `50`, group = zone_id, colour = "50")) +
    geom_line(data = Monthly_quantiles, aes(x = Month, y = `75`, group = zone_id, colour = "75")) +
    geom_line(data = Monthly_quantiles, aes(x = Month, y = `90`, group = zone_id, colour = "90")) +
  facet_wrap(~zone_id, ncol = 5) + 
  theme_bw() + 
  labs(title = "Monthly Energy Use By Zone",
       subtitle = "With Quantile Curves") + 
  scale_colour_manual(name = "Quantile", values = cols)+ 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") 
  

##Monthly_Scatter


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
Daily_quantiles <- All_data_tidy %>% filter(!is.na(kWh)) %>% 
  select(zone_id, Day, kWh) %>% group_by(zone_id, Day) %>% 
  summarize(`10` = quantile(kWh, probs = 0.1),
            `25` = quantile(kWh, probs = 0.25),
            `50` = quantile(kWh, probs = 0.5),
            `75` = quantile(kWh, probs = 0.75),
            `90` = quantile(kWh, probs = 0.9))

Daily_Scatter <- All_data_tidy %>% filter(!is.na(kWh)) %>% select(zone_id, Day, kWh) %>% 
  ggplot(aes(x = Day, y = kWh)) + geom_point(aes(group = zone_id)) + geom_jitter(size = .1, alpha = 0.1) + 
  geom_line(data = Daily_quantiles, aes(x = Day, y = `10`, group = zone_id, colour = "10")) +
  geom_line(data = Daily_quantiles, aes(x = Day, y = `25`, group = zone_id, colour = "25")) +
  geom_line(data = Daily_quantiles, aes(x = Day, y = `50`, group = zone_id, colour = "50")) +
  geom_line(data = Daily_quantiles, aes(x = Day, y = `75`, group = zone_id, colour = "75")) +
  geom_line(data = Daily_quantiles, aes(x = Day, y = `90`, group = zone_id, colour = "90")) +
  facet_wrap(~zone_id, ncol = 5) + 
  theme_bw() + 
  labs(title = "Zone Energy Use By Day of Week",
       subtitle = "With Quantile Curves") + 
  scale_colour_manual(name = "Quantile",  values = cols)+ 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") 

##Daily_Scatter

##this is much more interesting. 3,5,6,7,10,11,17,20 show what you would expect
##with the peaks in the middle

##Hour
Hourly_quantiles <- All_data_tidy %>% filter(!is.na(kWh)) %>% 
  select(zone_id, Hour, kWh) %>% group_by(zone_id, Hour) %>% 
  summarize(`10` = quantile(kWh, probs = 0.1),
            `25` = quantile(kWh, probs = 0.25),
            `50` = quantile(kWh, probs = 0.5),
            `75` = quantile(kWh, probs = 0.75),
            `90` = quantile(kWh, probs = 0.9))

Hourly_Scatter <- All_data_tidy %>% filter(!is.na(kWh)) %>% select(zone_id, Hour, kWh) %>% 
  ggplot(aes(x = Hour, y = kWh)) + geom_point(aes(group = zone_id)) + geom_jitter(size = .1, alpha = 0.1) + 
  geom_line(data = Hourly_quantiles, aes(x = Hour, y = `10`, group = zone_id, colour = "10")) +
  geom_line(data = Hourly_quantiles, aes(x = Hour, y = `25`, group = zone_id, colour = "25")) +
  geom_line(data = Hourly_quantiles, aes(x = Hour, y = `50`, group = zone_id, colour = "50")) +
  geom_line(data = Hourly_quantiles, aes(x = Hour, y = `75`, group = zone_id, colour = "75")) +
  geom_line(data = Hourly_quantiles, aes(x = Hour, y = `90`, group = zone_id, colour = "90")) +
  facet_wrap(~zone_id, ncol = 5) + 
  theme_bw() + 
  labs(title = "Zone Energy Use By Hour",
       subtitle = "With Quantile Curves") + 
  scale_colour_manual(name = "Quantile",  values = cols)+ 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") 

##Hourly_Scatter
##Also interesting

##attempt to decompose
Zone_1_decompose_add <- All_data_tsbl %>% filter(zone_id == 1, !is.na(kWh)) %>% select(Time_stamp, kWh) %>%
  as.ts() %>% decompose("additive")
plot(Zone_1_decompose_add)

Zone_1_decompose_mult <- All_data_tsbl %>% filter(zone_id == 1, !is.na(kWh)) %>% select(Time_stamp, kWh) %>%
  as.ts() %>% decompose("multiplicative")
plot(Zone_1_decompose_mult)





