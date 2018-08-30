library(tidyverse)
library(forecast)
library(tsibble)
library(lubridate)

Temp_zone <- All_data_tsbl %>% filter(!is.na(kWh)) %>% 
  select(kWh, Average, zone_id) %>% mutate(zone_id = factor(zone_id))

Temp_zone_lm <- lm(kWh ~ Average + zone_id, data = Temp_zone)
summary(Temp_zone_lm)
##R2 is fine, F is fine, the p-values on the coefficients are fine, but man are those residuals ugly.
##Let's see what happens if we square the average temp as the graphs suggest a parrobolic shape.
Temp_zone_lm_2 <- lm(kWh ~ I(Average^2) + zone_id, data = Temp_zone)
summary(Temp_zone_lm_2)


##Add in time related variables
##the hour vairable has some well known ramp up and down times in the industry
##it seems like a cubic could be used to model this
Temp_zone_lm_3 <- lm(kWh ~ I(Average^2) + factor(zone_id) + I(Hour^3) + factor(Day) + factor(Month), data = filter(All_data_tidy, !is.na(kWh)))
summary(Temp_zone_lm_3)
##Still not great, but we aren't expecting great. We can't add trend with this model
##which does seem to exist


##Try a tslm for the average temperature (this takes a while to run)
Temp_ts <- All_data_tidy %>% filter(!is.na(kWh), zone_id == 1) %>% 
  select(Average) %>% msts(seasonal.periods = c(24,8766), start = decimal_date(as_datetime("2004-01-01 01:00:00"
)))

Temp_tslm <- tslm(Average ~ trend + season, data = Temp_ts)

summary(Temp_tslm)
checkresiduals(Temp_tslm)


##Can we fourier this and make it better?
Temp_Ftslm <- tslm(Average ~ trend + fourier(Average, c(6,30)), data = Temp_ts)
summary(Temp_Ftslm)
checkresiduals(Temp_Ftslm)
##it runs faster, but I don't know if that's better
##	Breusch-Godfrey test for serial correlation of order up to 7614
## data:  Residuals from Linear regression model
## LM test = 37272, df = 7614, p-value < 2.2e-16

Temp_Ftslm2 <- tslm(Average ~ trend + fourier(Average, c(2,4)), data = Temp_ts)
summary(Temp_Ftslm2)
checkresiduals(Temp_Ftslm2)

##Breusch-Godfrey test for serial correlation of order up to 7614
##data:  Residuals from Linear regression model
##LM test = 37350, df = 7614, p-value < 2.2e-16

##I do not hate this. It's not perfect, but it may just work.
##should try fourier terms and tbats

Temp_tbats <- Temp_ts %>% tbats()
summary(Temp_tbats)
autoplot(Temp_tbats) + theme_bw()
checkresiduals(Temp_tbats) + theme_bw()
##not awesome. I would probably stick with the linear model, as
##TBATS is most useful when the seasonality can change (in this case it won't)

rm(msts_plt,Temp_kWH, temp_tslm, Temp_zone, Temp_zone_lm, Temp_zone_lm_2, Temp_zone_lm_3,
   Temp_tslm, Temp_ts)
