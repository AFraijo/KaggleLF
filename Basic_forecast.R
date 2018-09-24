library(tidyverse)
library(forecast)
library(tsibble)
library(lubridate)

options(scipen = 100000000)

##We have identified seasonality (daily, weekly, and yearly) as well
##as an impact by the temperature. Let's start by finding forecasting 
##the time series and then add the linear model for the temperature

Backcast_data <- All_data_tsbl %>% filter(is.na(kWh))

Zone_1_test <-  All_data_tidy %>% filter(zone_id == 1, Time_stamp >= as_datetime("2008-1-1 00:00:00"))

Zone_1_train <- All_data_tidy %>% filter(zone_id == 1, Time_stamp < as_datetime("2008-1-1 00:00:00"))

Zone_1_msts <- All_data_tidy %>% filter(zone_id == 1, Time_stamp < as_datetime("2008-1-1 00:00:00")) %>% select(kWh) %>% 
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


##xreg assumes the errors of a regression are ARIMA
Zone_1_xreg_check <- auto.arima(Zone_1_train[,"kWh"], xreg = Zone_1_train[,"Average"])
Zone_1_xreg_check %>% checkresiduals()

##Ljung-Box test
##data:  Residuals from Regression with ARIMA(2,1,3) errors
##Q* = 213.04, df = 4, p-value < 0.00000000000000022
##Model df: 6.   Total lags used: 10

##It's not bad! There are some lags that are high on the ACF plot, but the residuals look Gaussian
##Let's try a quadratic version
Zone_1_xreg_check2 <- auto.arima(Zone_1_train[,"kWh"], xreg = I(Zone_1_train[,"Average"]^2) + Zone_1_train[,"Average"])
Zone_1_xreg_check2 %>% checkresiduals()

Zone_1_xreg <- forecast(Zone_1_train[,"kWh"], xreg = Zone_1_test[,"Average"])





