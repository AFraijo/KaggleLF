library(tidyverse)
library(forecast)
library(tsibble)

Temp_zone <- All_data_tsbl %>% filter(!is.na(kWh)) %>% 
  select(kWh, Average, zone_id) %>% mutate(zone_id = factor(zone_id))

Temp_zone_lm <- lm(kWh ~ Average + zone_id, data = Temp_zone)
summary(Temp_zone_lm)
##R2 is fine, F is fine, the p-values on the coefficients are fine, but man are those residuals ugly.
##Let's see what happens if we square the average temp as the graphs suggest a parrobolic shape.
Temp_zone_lm_2 <- lm(kWh ~ Average^2 + zone_id, data = Temp_zone)
summary(Temp_zone_lm_2)

Temp_zone_tsbl <- All_data_tsbl %>% filter(!is.na(kWh)) %>% 
  select(Time_stamp, kWh, Average, zone_id) %>% mutate(zone_id = factor(zone_id)) %>%  as.ts()

Temp_zone_tslm <- tslm(kWh ~ trend + season + Average, data = Temp_zone_tsbl)
