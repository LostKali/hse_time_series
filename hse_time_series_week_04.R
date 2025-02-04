library(tidyverse) # обработка данных
library(fpp3) # куча плюшек для рядов
library(rio) # импорт данных

set.seed(777)
data = tibble(y = arima.sim(n = 120, model = list(ma = 0.9)))
data

data$date = yearmonth(ymd('2000-12-01') + months(1:120))
data

data = as_tsibble(data, index = date)
data


gg_tsdisplay(data, y)

ACF(data, y)
ACF(data, y) %>% autoplot()

PACF(data, y)
PACF(data, y) %>% autoplot()


# на коротких рядах оценка ACF начинает врать
data2 = tibble(y = arima.sim(n = 24, model = list(ma = 0.9)))
data2

data2$date = yearmonth(ymd('2000-12-01') + months(1:24))
data2

data2 = as_tsibble(data2, index = date)
data2


gg_tsdisplay(data2, y)

ACF(data2, y)
ACF(data2, y) %>% autoplot()

PACF(data2, y)
PACF(data2, y) %>% autoplot()


m = import("Documents/study/finances/hse/time series/marriages.csv")
glimpse(m)

m2 = mutate(m, date=yearmonth(date))
glimpse(m2)

marriages = as_tsibble(m2, index = date, key=c('region'))

marriages

rf = filter(marriages, region=='Российская Федерация')
gg_tsdisplay(rf, total)

rf_train = filter(rf, date < ymd('2019-09-01'))
tail(rf_train)
tail(rf)

mods = model(rf_train,
             snaive = SNAIVE(total),
             theta = THETA(total),
             ma12 = ARIMA(total ~ 1 + pdq(0, 0, 12) + PDQ(0, 0, 0)),
             stl_ma = decomposition_model(STL(total ~ season(window = Inf)),
                                          ARIMA(season_adjust ~ 1 + pdq(0, 0, 1:5) + PDQ(0, 0, 0)),
                                          SNAIVE(season_year)))

report(mods$ma12[[1]])
report(mods$stl_ma[[1]])

fcst = forecast(mods, h = '2 year')
accuracy(fcst, rf)
