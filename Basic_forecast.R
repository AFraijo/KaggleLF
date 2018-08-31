library(tidyverse)
library(forecast)
library(tsibble)
library(lubridate)

options(scipen = 100000000)

##We have identified seasonality (daily, weekly, and yearly) as well
##as an impact by the temperature. Let's start by finding forecasting 
##the time series and then add the linear model for the temperature

Backcast_data <- All_data_tsbl %>% filter(is.na(kWh))


Zone_1_msts <- All_data_tidy %>% filter(zone_id == 1, !is.na(kWh)) %>% select(kWh) %>% 
  msts(seasonal.periods = c(24,168,8766), start = decimal_date(as_datetime("2004-01-01 01:00:00"
  )))
##Zone_1_mstl <- Zone_1_msts %>% mstl(s.window = "periodic", robust = TRUE)
##Zone_1_mstl %>% seasadj() %>% naive() %>% autoplot() 
##Zone_1_mstl %>% forecast(method="naive", lambda = "auto") %>% autoplot()
##can use stlf instead. 

Zone_1_Naive <- Zone_1_msts %>% stlf(s.window = "periodic", robust = TRUE,method="naive", lambda = "auto")
Zone_1_Naive %>% autoplot(include = 12419, xlab = "Year", ylab = "Energy Demand (kWh)") + theme_bw()
##Need to figure out autoplot details and how to zoom in on backcast

Zone_1_ARIMA <- Zone_1_msts %>% stlf(s.window = "periodic", robust = TRUE,method="arima", lambda = "auto")
Zone_1_ARIMA %>% autoplot(include = 12419, xlab = "Year", ylab = "Energy Demand (kWh)") + theme_bw()


Zone_1_ETS <- Zone_1_msts %>% stlf(s.window = "periodic", robust = TRUE,method="ets", lambda = "auto")
Zone_1_ETS %>% autoplot(include = 12419, xlab = "Year", ylab = "Energy Demand (kWh)") + theme_bw()
