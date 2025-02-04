# install.packages("tidyverse")
# install.packages("fpp3")
library(tidyverse)
library(fpp3)

n_obs = 120
set.seed(777)
two = tsibble(date = yearmonth(ymd('2010-01-01') + months(0:(n_obs - 1))), 
              iid = rnorm(n_obs, mean = 10, sd = 4), 
              rwalk = 10 + cumsum(rnorm(n_obs, mean = 0, sd = 1)), 
              index = date)

library(ggplot2)
library(fabletools)

autoplot(two, iid)
autoplot(two, rwalk)

two

gg_season(two, rwalk)
gg_subseries(two, rwalk)

gg_lag(two, rwalk)

gg_lag(two, iid)

# TOP! 3 графика
gg_tsdisplay(two, rwalk, plot_type = 'season')

lake = as_tsibble(LakeHuron)
lake

two_old_ts = as.ts(two)
two_old_ts

# fedstat.ru/indicator/33553
# install.packages("rio")
library(rio)

d = import("Documents/study/finances/hse/time series/fedstat_33553.xlsx")
glimpse(d)

d2 = import("Documents/study/finances/hse/time series/fedstat_33553.xlsx", skip=2)
glimpse(d2)
head(d2)

colnames(d2)[1:2] = c('region', 'period')

unique(d2$period)

d3 = filter(d2, period %in% c("январь", "февраль", "март", "апрель", "май", 
                              "июнь", "июль", "август", "сентябрь", "октябрь",
                              "ноябрь", "декабрь"))

unique(d3$period)

month_dict = tibble(period = unique(d3$period), 
                    month_n = 1:12)

d4 = left_join(d3, month_dict, by='period')
head(d4)

d5 = select(d4, -period)
d5 <- d5 %>% fill(region, .direction = "down")

glimpse(d5)

d6 = pivot_longer(d5, cols=`2006`:`2024`,
                  names_to = 'year',
                  values_to = 'total')

glimpse(d6)

d7 = mutate(d6, date = yearmonth(paste0(year, '-', month_n)))
glimpse(d7)

d8 = select(d7, -month_n, -year)

marriages = as_tsibble(d8, index = date, key=c('region'))

marr_rf = filter(marriages, region=='Российская Федерация')
autoplot(marr_rf)

export(marriages, "Documents/study/finances/hse/time series/attempt_1.csv")
attempt_1 = import("Documents/study/finances/hse/time series/attempt_1.csv")
glimpse(attempt_1)

marriages_save = mutate(marriages,
                        date = as.Date(date))
export(marriages_save, "Documents/study/finances/hse/time series/marriages.csv")
m = import("Documents/study/finances/hse/time series/marriages.csv")
glimpse(m)

m2 = mutate(m, date=yearmonth(date))
glimpse(m2)


marriages2 = as_tsibble(m2, index = date, key=c('region'))
m_rf = filter(marriages2, region=='Российская Федерация')
gg_tsdisplay(m_rf, total, plot_type = 'season')

stl_model = model(m_rf, 
                  decomp = STL(
                    total ~ trend(window = 7) +
                      season(window = 100)))


components(stl_model)

components(stl_model) %>% autoplot()

#install.packages("ggrepel")
library(ggrepel)

marr_features = features(marriages, total,
                         feat_stl)
glimpse(marr_features)


ggplot(marr_features, 
       aes(
         x = trend_strength, 
         y = seasonal_strength_year,
         label = region)) +
  geom_point() + geom_text_repel()

