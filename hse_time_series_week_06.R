library(tidyverse) # обработка данных
library(fpp3) # куча плюшек для рядов
library(rio) # импорт данных

# install.packages("urca")
library(urca) # тесты

m = import("Documents/study/finances/hse/time series/marriages.csv")
glimpse(m)

m2 = mutate(m, date=yearmonth(date))
glimpse(m2)

marriages = as_tsibble(m2, index = date, key=c('region'))

marriages

rf = filter(marriages, region=='Российская Федерация')
gg_tsdisplay(rf, total)

sum(is.na(rf$total)) # смотрим количество пропущенных значений
rf <- na.omit(rf) # дропаем пропущенные значения

res_kpss = ur.kpss(rf$total, type = 'mu')
summary(res_kpss)
# H_0: ts = mu + stat
# H_a: ts = mu + stat + rw
# H_0 is rejected

res_adf = ur.df(rf$total, type='drift', selectlags = 'AIC')
summary(res_adf)
# H_0: non-stationary ts = ARIMA(p, 1, q) + trend
# H_a: ts = ARIMA(p, 0, q) + const

# H_0 is rejected

gg_tsdisplay(rf, total, 
             lag_max=30, plot_type='partial')

tail(rf)

train = filter(rf, date < ymd('2021-11-01'))
tail(train)

models = model(train,
               naive = SNAIVE(total),
               theta = THETA(total),
               auto = ARIMA(total), # под капотом запустится алгоритм Хандакара-Хиндмана
               sarima111_x11 = ARIMA(total ~ 0 + pdq(1, 1, 1) + PDQ(0:1, 1, 1))
               )

report(
  models$auto[[1]]
)

report(
  models$sarima111_x11[[1]]
)

fcst = forecast(models, h = '3 years')
accuracy(fcst, rf)

marr_slide = slide_tsibble(rf, .size = 60, .step = 4)
marr_slide

models_slide = model(marr_slide,
               naive = SNAIVE(total),
               theta = THETA(total),
               auto = ARIMA(total), # под капотом запустится алгоритм Хандакара-Хиндмана
)
mod_aggr = mutate(models_slide, 
                  average3 = (auto + naive + theta) / 3,
                  auto_theta = (auto + theta) / 2,
                  naive_theta = (naive + theta) / 2)

fcst_slide = forecast(mod_aggr, h = 1)
accuracy(fcst_slide, rf)