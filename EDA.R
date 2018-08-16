##Opening EDA work
##Author: Me

##Strategy: Let's start by graphing the timeseries at various levels
##Weekdays
##Months
##Hour of day

library(tidyverse)
library(tsibble)
library(lubridate)

##library(feather)
##read_feather("All_data_tidy.feather")

##Ok, so after all that we actually need to add hour, month, and weekday back in
##so that we can roll up to those