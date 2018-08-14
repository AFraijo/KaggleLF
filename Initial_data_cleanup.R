##New Try, years later
##Author: Me

library(tidyverse)
library(lubridate)
library(feather)

load("~/Projects/KaggleLF/Load_Forecasting.Rproj.RData")

##this is not in a tidy format which makes some things difficult
##change date format

create_date <- function(YEAR, MONTH, DAY){
  date <- paste(YEAR, MONTH, DAY, sep="-") %>% ymd()
  return(date)
}

Load_history <- Load_history %>% 
  mutate(Date = create_date(year, month, day))