library(tidyverse) # обработка данных
library(fpp3) # куча плюшек для рядов
library(rio) # импорт данных

set.seed(777)

data = tibble(a = arima.sim(n = 100,
                            model = list(ar = 0.5)),
              b = arima.sim(n = 100,
                            model = list(ar = 0.9)),
              c = cumsum(rnorm(n = 100, mean = 0, sd = 1)))

data$year = 2001:2100
data = as_tsibble(data, index = year)

gg_tsdisplay(data, a, plot_type = 'partial')


gg_tsdisplay(data, b, plot_type = 'partial')

gg_tsdisplay(data, c, plot_type = 'partial')

train = filter(data, year < 2081)

mod_b = model(train,
              ar1 = ARIMA(b ~ pdq(1, 0, 0)),
              ma1 = ARIMA(b ~ pdq(0, 0, 1)),
              naive = NAIVE(b))

mod_c = model(train,
              ar1 = ARIMA(c ~ pdq(1, 0, 0)),
              ma1 = ARIMA(c ~ pdq(0, 0, 1)),
              naive = NAIVE(c))


fcst_b = forecast(mod_b, h = 20)
fcst_c = forecast(mod_c, h = 20)

autoplot(fcst_b, filter(data, year > 2050))

autoplot(fcst_c, filter(data, year > 2050))

m = import("Documents/study/finances/hse/time series/marriages.csv")
glimpse(m)

m2 = mutate(m, date_round = floor_date(date, unit = 'year'),
            year = year(date_round))
glimpse(m2)


m3 = select(m2, -date, -date_round)
m_agg = group_by(m3, region, year) %>%
  summarise(sum = sum(total),
            max = max(total),
            .groups = 'keep')

glimpse(m_agg)

rfy = filter(m_agg, region == 'Российская Федерация')
rfy = as_tsibble(rfy, index = year)

gg_tsdisplay(rfy, sum, plot_type = 'partial')

train = filter(rfy, year < 2019)

models = model(train,
               ar1 = ARIMA(sum ~ pdq(1, 0, 0)),
               ma1 = ARIMA(sum ~ pdq(0, 0, 1)),
               naive = NAIVE(sum),
               arma11 = ARIMA(sum ~ pdq(1, 0, 1)))

models

report(models$ar1[[1]])

nrow(train)
nrow(rfy)

fcst = forecast(models, h = 6)

accuracy(fcst, rfy, )
