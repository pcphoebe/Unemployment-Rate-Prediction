### Using Google Trends Keywords to Predict U.S. Unemployment Rates ###
## Group 10: King of Data ##
# This file includes our final code for the project #

library(ggplot2);library(ggthemes);library(gridExtra) # Visualization
library(quantmod);library(xts);library(zoo);library(tseries) # time-series
library(forecast);library(fpp); library(fpp2) # time-series forecast
library(dplyr);library(psych) # data wrangling
library(gtrendsR) # Google Trends API

# Compiling list of keywords and topic: unemployment from gtrends
keyword1 = gtrends(keyword = 'unemployment', geo = c('US'), time = '2004-01-01 2020-12-31', tz = -300)
keyword2 = gtrends(keyword = 'compensation', geo = c('US'), time = '2004-01-01 2020-12-31', tz = -300)
keyword3 = gtrends(keyword = 'jobs', geo = c('US'), time = '2004-01-01 2020-12-31', tz = -300)
keyword4 = gtrends(keyword = 'salary', geo = c('US'), time = '2004-01-01 2020-12-31', tz = -300)
keyword5 = gtrends(keyword = 'claims', geo = c('US'), time = '2004-01-01 2020-12-31', tz = -300)
topic = gtrends(keyword = '/m/07s_c', geo = c('US'), time = '2004-01-01 2020-12-31', tz = -300)
unemployment_out1 = keyword1$interest_over_time[1:2]
unemployment_out2 = keyword2$interest_over_time[1:2]
unemployment_out3 = keyword3$interest_over_time[1:2]
unemployment_out4 = keyword4$interest_over_time[1:2]
unemployment_out5 = keyword5$interest_over_time[1:2]
topic_out1 = topic$interest_over_time[1:2]
names(unemployment_out1)[names(unemployment_out1) == "hits"] <- "unemployment"
names(unemployment_out2)[names(unemployment_out2) == "hits"] <- "compensation"
names(unemployment_out3)[names(unemployment_out3) == "hits"] <- "jobs"
names(unemployment_out4)[names(unemployment_out4) == "hits"] <- "salary"
names(unemployment_out5)[names(unemployment_out5) == "hits"] <- "claims"
names(topic_out1)[names(topic_out1) == "hits"] <- "topic"
merge=merge(x = unemployment_out1, y = unemployment_out2, by = "date", all = TRUE)
merge=merge(x = merge, y = unemployment_out3, by = "date", all = TRUE)
merge=merge(x = merge, y = unemployment_out4, by = "date", all = TRUE)
merge=merge(x = merge, y = unemployment_out5, by = "date", all = TRUE)
merge=merge(x = merge, y = topic_out1, by = "date", all = TRUE)
merge$date=as.Date(merge$date)

# Manually procure unemployment rate from FRED St. Louis
# read the UNRATE.csv file in dir
unrate=read.csv("UNRATE.csv")
names(unrate)[names(unrate) == "DATE"] <- "date"
# slicing the unemployment rate length to match gtrends
x=unrate[c(673:876),]
merge$unemploymentRate=x$UNRATE
merge$unemploymentRate=as.double(merge$unemploymentRate)
# find and drop null value
data <- na.omit(merge)
# reading original rate as separate variable
og_Rate = read.csv("UNRATE.csv")

# converting to time series
unemployment = ts(data=data$unemployment,start = c(2004,01),frequency = 12)
compensation = ts(data=data$compensation,start = c(2004,01),frequency = 12)
jobs = ts(data=data$jobs,start = c(2004,01),frequency = 12)
salary = ts(data=data$salary,start = c(2004,01),frequency = 12)
claims = ts(data=data$claims,start = c(2004,01),frequency = 12)
topic = ts(data=data$topic,start = c(2004,01),frequency = 12)
unemploymentRate = ts(data=data$unemploymentRate,start = c(2004,01),frequency = 12)


# plotting y series
library(ggplot2)
autoplot(unemploymentRate)
autoplot(diff(unemploymentRate,1))
autoplot(og_rate)
autoplot(diff(og_rate,1))

ndiffs(diff(unemploymentRate,1))
nsdiffs(unemploymentRate)
# ggAcf
library(forecast)
ggAcf(x = unemploymentRate)

# plotting rate + keywords
library(fpp)
library(gridExtra)
grid.arrange(autoplot(unemploymentRate),
             autoplot(unemployment),
             autoplot(compensation),
             autoplot(jobs),
             autoplot(salary),
             autoplot(claims)
             )
# seasonal plot
ggseasonplot(unemploymentRate)
ggseasonplot(unemployment)
ggseasonplot(compensation)
ggseasonplot(jobs)
ggseasonplot(salary)
ggseasonplot(claims)

# breaking down components
unemploymentRate%>%
  stl(s.window = 'periodic')%>%
  autoplot()

# splitting data
rate_train = window(unemploymentRate,start=c(2004,01), end=c(2016,12))
rate_test = window(unemploymentRate, start=c(2017,01), end=c(2020,12))
length(rate_train) # 156 
length(rate_test) # 48
length(rate_train)/(length(rate_train)+length(rate_test)) # 76% in train

# splitting x series
unemployment_train = window(unemployment,start=c(2004,01), end=c(2016,12))
unemployment_test = window(unemployment, start=c(2017,01), end=c(2020,12))
compensation_train = window(compensation,start=c(2004,01), end=c(2016,12))
compensation_test = window(compensation, start=c(2017,01), end=c(2020,12))
jobs_train = window(jobs,start=c(2004,01), end=c(2016,12))
jobs_test = window(jobs, start=c(2017,01), end=c(2020,12))
salary_train = window(salary,start=c(2004,01), end=c(2016,12))
salary_test = window(salary, start=c(2017,01), end=c(2020,12))
claims_train = window(claims,start=c(2004,01), end=c(2016,12))
claims_test = window(claims, start=c(2017,01), end=c(2020,12))
topic_train = window(topic,start=c(2004,01), end=c(2016,12))
topic_test = window(topic, start=c(2017,01), end=c(2020,12))

# holt's: worst, no trend
holt_model = holt(rate_train,h=48)
forecast::accuracy(holt_model, x = unemploymentRate)
# average method
average_model = meanf(rate_train,h = 48)
forecast::accuracy(average_model, x = unemploymentRate)
# Holt-Winters Multiplicative
hw_multiplicative = hw(rate_train,h=48,seasonal = 'multiplicative', damped=T)
forecast::accuracy(hw_multiplicative, x = unemploymentRate)
# Holt-Winter Additive
hw_additive = hw(rate_train,h=48,seasonal = 'additive', damped=T)
forecast::accuracy(hw_additive, x = unemploymentRate)
# Holt damp
holt_damped_model = holt(rate_train,h=48,damped = T)
forecast::accuracy(holt_damped_model, x = unemploymentRate)
# drift
drift_model = rwf(rate_train,h=48,drift = T)
forecast::accuracy(drift_model,x = unemploymentRate)
# naive method
naive_model = naive(rate_train,h=48)
forecast::accuracy(naive_model, x = unemploymentRate)
# ses
ses_model = ses(rate_train,h = 48)
forecast::accuracy(ses_model,x = unemploymentRate) 
# seasonal naive
seasonal_naive_model = snaive(rate_train,h=48)
forecast::accuracy(seasonal_naive_model, x = unemploymentRate)
# ETS AAA
ets_aaa = ets(rate_train,model = 'AAA')
ets_aaa_forecast = forecast(ets_aaa,h=48)
ets_aaa_forecast
forecast::accuracy(ets_aaa_forecast, x = unemploymentRate)
# residuals
checkresiduals(ets_aaa) # Ljung-box significant; trans needed
ggAcf(ets_aaa$residuals)
# ETS auto
ets_auto = ets(rate_train)
ets_auto_forecast = forecast(ets_auto,h=48)
forecast::accuracy(ets_auto_forecast, x = unemploymentRate)
# residuals
checkresiduals(ets_auto_forecast) # Ljung-box still significant
ggAcf(ets_auto_forecast$residuals)

# as table: simple and exponential models
rbind(holt = forecast::accuracy("object" = holt_model,x = unemploymentRate)[2,],
      average_model = forecast::accuracy("object" = average_model, x = unemploymentRate)[2,],
      hw_multiplicative = forecast::accuracy("object" = hw_multiplicative,x = unemploymentRate)[2,],
      hw_additive = forecast::accuracy("object" = hw_additive,x = unemploymentRate)[2,],
      holt_damped_model = forecast::accuracy("object" = holt_damped_model,x = unemploymentRate)[2,],
      drift_model = forecast::accuracy("object" = drift_model,x = unemploymentRate)[2,],
      naive_model = forecast::accuracy("object" = naive_model,x = unemploymentRate)[2,],
      ses_model = forecast::accuracy("object" = ses_model,x = unemploymentRate)[2,],
      seasonal_naive_model = forecast::accuracy("object" = seasonal_naive_model,x = unemploymentRate)[2,],
      ets_aaa = forecast::accuracy("object" = ets_aaa_forecast,x = unemploymentRate)[2,],
      ets_auto = forecast::accuracy("object" = ets_auto_forecast,x = unemploymentRate)[2,]
)%>% 
  as.data.frame() %>% 
  arrange(RMSE)

# as table: simple
rbind(average_model = forecast::accuracy("object" = average_model, x = unemploymentRate)[2,],
      naive_model = forecast::accuracy("object" = naive_model,x = unemploymentRate)[2,],
      seasonal_naive_model = forecast::accuracy("object" = seasonal_naive_model,x = unemploymentRate)[2,],
      drift_model = forecast::accuracy("object" = drift_model,x = unemploymentRate)[2,]
)%>% 
  as.data.frame() %>% 
  arrange(RMSE)
# visualizing forecasts: simple
autoplot(rate_train, color='sienna')+
  autolayer(average_model,series = 'Average Model',PI=F)+
  autolayer(naive_model,series = 'Naive Model',PI=F)+
  autolayer(seasonal_naive_model,series = 'Seasonal Naive Model',PI=F)+
  autolayer(drift_model,series = 'Drift Model',PI=F)+
  autolayer(rate_test, series = 'rate_test', color='grey60')
  
# as table: exponential
rbind(holt = forecast::accuracy("object" = holt_model,x = unemploymentRate)[2,],
      holt_damped_model = forecast::accuracy("object" = holt_damped_model,x = unemploymentRate)[2,],
      hw_additive = forecast::accuracy("object" = hw_additive,x = unemploymentRate)[2,],
      hw_multiplicative = forecast::accuracy("object" = hw_multiplicative,x = unemploymentRate)[2,],
      ses_model = forecast::accuracy("object" = ses_model,x = unemploymentRate)[2,],
      ets_aaa = forecast::accuracy("object" = ets_aaa_forecast,x = unemploymentRate)[2,],
      ets_auto = forecast::accuracy("object" = ets_auto_forecast,x = unemploymentRate)[2,]
)%>% 
  as.data.frame() %>% 
  arrange(RMSE)

# visualizing forecasts: exponential
autoplot(rate_train)+
  autolayer(holt_model,series = "Holt's Model",PI=F)+
  autolayer(holt_damped_model,series = "Holt's damped model",PI=F)+
  autolayer(hw_additive,series = 'Holt-Winter additive',PI=F)+
  autolayer(hw_multiplicative,series = 'Holt-Winter multiplicative',PI=F)+
  autolayer(ses_model,series = 'Simple Exponential Smoothing',PI=F)+
  autolayer(ets_aaa_forecast,series = 'ets AAA',PI=F)+
  autolayer(ets_auto_forecast,series = 'ets auto',PI=F,color='red')+
  autolayer(rate_test, color='grey60')
autoplot(rate_train)+
  autolayer(holt_damped_model,series = "Holt's damped model",PI=F)+
  autolayer(hw_additive,series = 'Holt-Winter additive',PI=F)+
  autolayer(hw_multiplicative,series = 'Holt-Winter multiplicative',PI=F)+
  autolayer(ets_auto_forecast,series = 'ets auto',PI=F)+
  autolayer(rate_test, color='grey60')

# ARIMA
# Transformation on rate_train needed
autoplot(rate_train)
rate_train %>%
  ggtsdisplay()

# stabilize variance
rate_const_var = BoxCox(rate_train,lambda = BoxCox.lambda(rate_train))
# check series
ndiffs(rate_const_var) # 2
nsdiffs(rate_const_var) # seasonal = 0
autoplot(rate_const_var)
ggAcf(rate_const_var)
pacf(rate_const_var)

# auto.arima(): ARIMA(1,1,2)(2,0,1)[12] rmse 2.5297483
arima_auto = auto.arima(y = rate_train, ic = c("aicc"), stepwise = F,approximation = F, lambda = BoxCox.lambda(rate_train))
arima_auto_forecast = forecast(arima_auto,h=48)
forecast::accuracy(arima_auto_forecast, x = unemploymentRate)
checkresiduals(arima_auto) # significant spike at lag 5; p = 0.04779
# copied from above in case changes
arima_auto = Arima(rate_train, order = c(1,1,2), seasonal = c(2,0,1), lambda = BoxCox.lambda(rate_train))
arima_auto_forecast = forecast(arima_auto, h = 48)
forecast::accuracy(arima_auto_forecast, x = unemploymentRate)
checkresiduals(arima_auto) # p = 0.04779

# Manual fit fixing lag 5 for rate: rmse 2.4582818
rate_arima = Arima(rate_train, order = c(1,1,5), seasonal = c(2,0,1), lambda = BoxCox.lambda(rate_train))
rate_arima_forecast = forecast(rate_arima, h = 48)
forecast::accuracy(rate_arima_forecast, x = unemploymentRate)
checkresiduals(rate_arima) # p = 0.4326

# keyword as x series
# unemployment: rmse 2.4901022
key_unemployment = Arima(rate_train, order = c(1,1,5), seasonal = c(2,0,1), xreg = unemployment_train, lambda = BoxCox.lambda(rate_train))
key_unemployment_forecast = forecast(key_unemployment, h = 48, xreg = unemployment_test)
forecast::accuracy(key_unemployment_forecast, x = unemploymentRate)
checkresiduals(key_unemployment) # residuals: p = 0.3648

# claims: rmse 2.4662552
key_claims = Arima(rate_train, order = c(1,1,5), seasonal = c(2,0,1), xreg = claims_train, lambda = BoxCox.lambda(rate_train))
key_claims_forecast = forecast(key_claims, h = 48, xreg = claims_train)
forecast::accuracy(key_claims_forecast, x = unemploymentRate)
checkresiduals(key_claims) # p = 0.3696

# compensation: rmse 2.4579468
key_compensation = Arima(rate_train, order = c(1,1,5), seasonal = c(2,0,1), xreg = compensation_train, lambda = BoxCox.lambda(rate_train))
key_compensation_forecast = forecast(key_compensation, h = 48, xreg = compensation_train)
forecast::accuracy(key_compensation_forecast, x = unemploymentRate)
checkresiduals(key_compensation) # p = 0.3589

# jobs: rmse 2.4497756
key_jobs = Arima(rate_train, order = c(1,1,5), seasonal = c(2,0,1), xreg = jobs_train, lambda = BoxCox.lambda(rate_train))
key_jobs_forecast = forecast(key_jobs, h = 48, xreg = jobs_test)
forecast::accuracy(key_jobs_forecast, x = unemploymentRate)
checkresiduals(key_jobs) # p = 0.3162

# salary: rmse 2.4530360
key_salary = Arima(rate_train, order = c(1,1,5), seasonal = c(2,0,1), xreg = salary_train, lambda = BoxCox.lambda(rate_train))
key_salary_forecast = forecast(key_salary, h = 48, xreg = salary_train)
forecast::accuracy(key_salary_forecast, x = unemploymentRate)
checkresiduals(key_salary) # p = 0.3927

# topic: rmse 2.4843083
key_topic = Arima(rate_train, order = c(1,1,5), seasonal = c(2,0,1), xreg = topic_train, lambda = BoxCox.lambda(rate_train))
key_topic_forecast = forecast(key_topic, h = 48, xreg = topic_test)
forecast::accuracy(key_topic_forecast, x = unemploymentRate)
checkresiduals(key_topic) # p = 0.3026

# as table: keywords
rbind(unemployment_rate = forecast::accuracy("object" = rate_arima_forecast,x = unemploymentRate)[2,],
      key_unemployment = forecast::accuracy("object" = key_unemployment_forecast,x = unemploymentRate)[2,],
      key_claims = forecast::accuracy("object" = key_claims_forecast,x = unemploymentRate)[2,],
      key_compensation = forecast::accuracy("object" = key_compensation_forecast,x = unemploymentRate)[2,],
      key_jobs = forecast::accuracy("object" = key_jobs_forecast,x = unemploymentRate)[2,],
      key_salary = forecast::accuracy("object" = key_salary_forecast,x = unemploymentRate)[2,],
      key_topic = forecast::accuracy("object" = key_topic_forecast,x = unemploymentRate)[2,]
)%>% 
  as.data.frame() %>% 
  arrange(RMSE)

# arima visualization
autoplot(rate_train, color='sienna')+
  autolayer(rate_test,size=1.05,color='gray60')+
  autolayer(rate_arima_forecast,series = 'rate_arima',PI=F)

# keywords
autoplot(rate_train, color='sienna')+
  autolayer(rate_test,size=1.05,color='gray60')+
  autolayer(rate_arima_forecast,series = 'rate_arima',PI=F)+
  autolayer(key_unemployment_forecast,series = 'key_unemployment',PI=F)+
  autolayer(key_claims_forecast,series = 'key_claims',PI=F)+
  autolayer(key_compensation_forecast,series = 'key_compensation',PI=F)+
  autolayer(key_jobs_forecast,series = 'key_jobs',PI=F)+
  autolayer(key_salary_forecast,series = 'key_salary',PI=F)+
  autolayer(key_topic_forecast,series = 'key_topic',PI=F)

# Multiple layer CNN
library(h2o)
library(Metrics)
h2o.init()
set.seed(1031)

data2 = data

split = sample(x = c('train','validation','test'),size = nrow(data2),replace = T,prob = c(0.4,0.4,0.2))
train = data2[split=='train',]
validation = data2[split=='validation',]
test = data2[split=='test',]
train_h2o = as.h2o(train)
test_h2o = as.h2o(test)
validation_h2o = as.h2o(validation)
hyper_parameters = list(hidden=list(c(20,20),c(50,50),c(100,100,100), c(30,30,30),c(50,50,50,50),c(25,25,25,25)),
                        l1=seq(0,1e-4,1e-5),
                        l2=seq(0,1e-4,1e-5))

search_criteria = list(strategy='RandomDiscrete',
                       max_runtime_secs=1000,
                       max_models=1000,
                       seed=1031,
                       stopping_rounds=5,
                       stopping_tolerance=1e-2)
grid = h2o.grid(algorithm='deeplearning',
                training_frame = train_h2o,
                validation_frame=validation_h2o,
                x=3,
                y=8,
                epochs=10,
                stopping_metric='rmse',
                stopping_tolerance=1e-2,
                stopping_rounds=5,
                hyper_params = hyper_parameters,
                search_criteria = search_criteria)
best_model <- h2o.getModel(grid@model_ids[[1]])

grid2 = h2o.grid(algorithm='deeplearning',
                 training_frame = train_h2o,
                 validation_frame=validation_h2o,
                 x=4,
                 y=8,
                 epochs=10,
                 stopping_metric='rmse',
                 stopping_tolerance=1e-2,
                 stopping_rounds=5,
                 hyper_params = hyper_parameters,
                 search_criteria = search_criteria)
best_model2 <- h2o.getModel(grid2@model_ids[[1]])

grid3 = h2o.grid(algorithm='deeplearning',
                 training_frame = train_h2o,
                 validation_frame=validation_h2o,
                 x=5,
                 y=8,
                 epochs=10,
                 stopping_metric='rmse',
                 stopping_tolerance=1e-2,
                 stopping_rounds=5,
                 hyper_params = hyper_parameters,
                 search_criteria = search_criteria)
best_model3 <- h2o.getModel(grid3@model_ids[[1]])

grid4 = h2o.grid(algorithm='deeplearning',
                 training_frame = train_h2o,
                 validation_frame=validation_h2o,
                 x=6,
                 y=8,
                 epochs=10,
                 stopping_metric='rmse',
                 stopping_tolerance=1e-2,
                 stopping_rounds=5,
                 hyper_params = hyper_parameters,
                 search_criteria = search_criteria)
best_model4 <- h2o.getModel(grid4@model_ids[[1]])

grid5 = h2o.grid(algorithm='deeplearning',
                 training_frame = train_h2o,
                 validation_frame=validation_h2o,
                 x=7,
                 y=8,
                 epochs=10,
                 stopping_metric='rmse',
                 stopping_tolerance=1e-2,
                 stopping_rounds=5,
                 hyper_params = hyper_parameters,
                 search_criteria = search_criteria)
best_model5 <- h2o.getModel(grid5@model_ids[[1]])


pred = h2o.predict(best_model,test_h2o)
rmse(pred[1],test_h2o$unemploymentRate)

pred2 = h2o.predict(best_model2,test_h2o)
rmse(pred2[1],test_h2o$unemploymentRate)

pred3 = h2o.predict(best_model3,test_h2o)
rmse(pred3[1],test_h2o$unemploymentRate)

pred4 = h2o.predict(best_model4,test_h2o)
rmse(pred4[1],test_h2o$unemploymentRate)

pred5 = h2o.predict(best_model5,test_h2o)
rmse(pred5[1],test_h2o$unemploymentRate)


grid6 = h2o.grid(algorithm='deeplearning',
                 training_frame = train_h2o,
                 validation_frame=validation_h2o,
                 x=3:7,
                 y=8,
                 epochs=10,
                 stopping_metric='rmse',
                 stopping_tolerance=1e-2,
                 stopping_rounds=5,
                 hyper_params = hyper_parameters,
                 search_criteria = search_criteria)
best_model6 <- h2o.getModel(grid6@model_ids[[1]])

pred6 = h2o.predict(best_model6,test_h2o)
rmse(pred6[1],test_h2o$unemploymentRate)

best_model6
best_model6