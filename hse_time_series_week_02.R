library(tidyverse) # обработка данных
library(fpp3) # куча плюшек для рядов
library(rio) # импорт данных

m = import("Documents/study/finances/hse/time series/marriages.csv")
glimpse(m)

m2 = mutate(m, date=yearmonth(date))
glimpse(m2)

marriages2 = as_tsibble(m2, index = date, key=c('region'))

marriages2

m_rf = filter(marriages2, region=='Российская Федерация')
gg_tsdisplay(m_rf, total, plot_type = 'season')

unique(marriages2$date)

train = filter(marr_rf, date < ymd('2022-01-01'))
test = filter(marr_rf, date >= ymd('2022-01-01'))

model_table = model(train,
                    snaive = SNAIVE(total),
                    ets_aaa = ETS(total ~ error('A') + trend('A') + season('A')),
                    ets_ana = ETS(total ~ error('A') + trend('N') + season('A')),
                    ets_aaa_ln = ETS(log(total) ~ error('A') + trend('A') + season('A')),
                    ets_ana_ln = ETS(log(total) ~ error('A') + trend('N') + season('A'))
                    )

model_table

report(select(model_table, ets_aaa))

report(select(model_table, ets_ana))

fcst = forecast(model_table, h = '3 years')
fcst

marr_rf_vis = filter(marr_rf, date >= ymd('2017-01-01'))

fcst_sub = filter(fcst, .model %in% c('snaive', 'ets_aaa'))

autoplot(fcst_sub, marr_rf_vis)

accuracy(fcst, test)

accuracy(fcst, marr_rf)

model_table2 = mutate(model_table,
                      top2 = (ets_aaa_ln + snaive) / 2,
                      top3 = (ets_aaa_ln + snaive + ets_ana_ln) / 3)

fcst2 = forecast(model_table2, h = '3 years')
fcst2

accuracy(fcst2, marr_rf)

nrow(marr_rf)

marr_stretch = stretch_tsibble(marr_rf, 
                               .init = 110, .step = 1)
marr_stretch

model_table3 = model(marr_stretch,
                    snaive = SNAIVE(total),
                    ets_aaa = ETS(total ~ error('A') + trend('A') + season('A')),
                    ets_ana = ETS(total ~ error('A') + trend('N') + season('A')),
                    ets_aaa_ln = ETS(log(total) ~ error('A') + trend('A') + season('A')),
                    ets_ana_ln = ETS(log(total) ~ error('A') + trend('N') + season('A'))
)

model_table3

fcst3 = forecast(model_table3, h = 1)

accuracy(fcst3, marr_rf)

model_table4 = mutate(model_table3,
                     top2 = (ets_ana_ln + ets_aaa_ln) / 2,
                     top3 = (ets_ana_ln + ets_aaa_ln + ets_ana) / 3
)

fcst4 = forecast(model_table4, h = 1)
accuracy(fcst4, marr_rf)
