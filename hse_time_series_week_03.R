library(tidyverse) # обработка данных
library(fpp3) # куча плюшек для рядов
library(rio) # импорт данных

m = import("Documents/study/finances/hse/time series/marriages.csv")
glimpse(m)

m2 = mutate(m, date=yearmonth(date))
glimpse(m2)

marriages = as_tsibble(m2, index = date, key=c('region'))

marriages

m_rf = filter(marriages, region=='Российская Федерация')
gg_tsdisplay(m_rf, total)

tail(m_rf)

rf_train = filter(m_rf, date < ymd('2022-01-01'))
tail(rf_train)

mods = model(rf_train,
             snaive = SNAIVE(total),
             ana = ETS(total ~ error('A') + trend('N') + season('A')),
             aada = ETS(total ~ error('A') + trend('Ad') + season('A')),
             zzz = ETS(total),
             azz = ETS(total ~ error('A')),
             theta = THETA(total)
             )

mods

report(mods$zzz[[1]])
report(mods$theta[[1]])

fcst = forecast(mods, h = '3 years')

accuracy(fcst, m_rf)

mods2 = model(rf_train,
             snaive = SNAIVE(total),
             theta = THETA(total),
             cpmst = decomposition_model(
               STL(total ~ season(window = Inf)),
               ETS(season_adjust ~ error('A') + trend('Ad') + season('N')),
               SNAIVE(season_year)
             )
)

mods2
report(mods2$theta[[1]])
report(mods2$cpmst[[1]])


fcst2 = forecast(mods2, h = '3 years')
accuracy(fcst2, m_rf)

m_train = filter(marriages, date < ymd('2022-01-01'))
tail(m_train)

progressr::handlers(global = TRUE)
mods3 = model(m_train,
              snaive = SNAIVE(total),
              zzz = ETS(log(total)),
              theta = THETA(total)
              )

mods3

fcst3 = forecast(mods3,  h = '3 years')
acc = accuracy(fcst3, marriages)
acc

acc %>% group_by(.model) %>% summarise(av_mae = mean(MAE, na.rm = TRUE))
