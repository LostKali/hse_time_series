library(fpp3)
#install.packages("caret")
library(caret) # для оценки большого количество моделеле ML
#install.packages("ranger")
library(ranger)

air = as_tsibble(AirPassengers)
air
gg_tsdisplay(air)

air2 = mutate(air, 
              ln_pass = log(value),
              t = 1:nrow(air))
air2

#install.packages("forecast")
fourier_x = forecast::fourier(AirPassengers,  K = 2)

fourier_x

colnames(fourier_x) = c('s1', 'c1', 's2', 'c2')

air3 = bind_cols(air2, fourier_x)
air3

tail(air3)

air3test = tail(air3, 24)
air3train = head(air3, -24)

cv_params = trainControl(method = 'cv',
                         number = 5)
ols = train(ln_pass ~ t + s1 + c1 + s2 + c2,
            data = air3train,
            trControl = cv_params,
            method = 'lm')

rf = train(ln_pass ~ t + s1 + c1 + s2 + c2,
            data = air3train,
            trControl = cv_params,
            method = 'ranger',
           num.trees = 10000)

#install.packages("xgboost")
gb = train(ln_pass ~ t + s1 + c1 + s2 + c2,
            data = air3train,
            trControl = cv_params,
            method = 'xgbTree')

ols_fcst = predict(ols, air3test)
rf_fcst = predict(rf, air3test)
gb_fcst = predict(gb, air3test)

air4test = mutate(air3test,
                  ols = ols_fcst,
                  rf = rf_fcst,
                  gb = gb_fcst)

glimpse(air4test)


library(fpp3)
library(tsibbledata)

head(vic_elec)

elec = index_by(vic_elec, Date) %>%
  summarise(dem = mean(Demand),
            temp = mean(Temperature))

gg_tsdisplay(elec)
gg_tsdisplay(tail(elec, 60))

elec_train = head(elec, -60)
elec_test = tail(elec, 60)

mods = model(elec_train,
             naive = NAIVE(dem),
             arimaf = ARIMA(
               dem ~ fourier(K = 3) + PDQ(0, 0, 0)))
report(mods$arimaf[[1]])

fcst = forecast(mods, h=60)
autoplot(fcst)

accuracy(fcst, elec)

#install.packages("ARDL")
library(ARDL) # основное использование - выяснение связей между переменными
denmark
glimpse(denmark)

den = as_tibble(denmark)
den

den2 = mutate(den, 
              quarter = yearquarter(time(denmark)))
den2
glimpse(den2)
den2 = as_tsibble(den2, index = quarter)
gg_tsdisplay(den2, LRM)

ardl1 = ardl(data = denmark,
             LRM ~ LRY + IBO + IDE,
             order = c(2, 1, 2, 2))
ardl1

ardl_many = auto_ardl(data = denmark,
                      LRM ~ LRY + IBO + IDE,
                      max_order = 3)

ardl_many

ardl_many$best_model