library("readxl")
library("fpp2")
library("xts")
library("forecast")
library(IRdisplay)
library(magrittr)
library(tidyverse)
library(scales)
library(gridExtra)
library(forecast)
library(tseries)
library(ggthemes)
library(tsibbledata)
library(pastecs)
library(Metrics)
library(plm)

dt <- read_excel("DATASET.xls")
head(dt)

###Plot 5 variable and basic stat
plot(x = dt$observation_date, y = dt$SP500, type = "l")
plot(x = dt$observation_date, y = dt$DJIA, type = "l")
plot(x = dt$observation_date, y = dt$`10 year break inflation rate`, type = "l")
plot(x = dt$observation_date, y = dt$NASDAQCOM, type = "l")
plot(x = dt$observation_date, y = dt$VIXCLS, type = "l")

##Basic stats for the above variables
summary(dt)

var(dt$SP500)
var(dt$DJIA)
var(dt$`10 year break inflation rate`)
var(dt$NASDAQCOM)
var(dt$VIXCLS)

cov(dt$SP500,dt$DJIA)
cov(dt$SP500,dt$`10 year break inflation rate`)
cov(dt$SP500,dt$NASDAQCOM)
cov(dt$SP500,dt$VIXCLS)

#Constructing Time Series for the data sample
T_sample_SP500 <- dt$SP500
R_sample_SP500 <- seq(as.Date("2018-01-01"),as.Date("2019-12-30"),by= "weeks")
Sp500_R <- zoo(T_sample_SP500, R_sample_SP500)
R_SP500 = as.ts(Sp500_R)
R_SP500

T_sample_DJIA <- dt$DJIA
R_sample_DJIA <- seq(as.Date("2018-01-01"),as.Date("2019-12-30"),by= "weeks")
DJIA_R <- zoo(T_sample_DJIA, R_sample_DJIA)
R_DJIA = as.xts(DJIA_R)
R_DJIA

T_sample_InF <- dt$`10 year break inflation rate`
R_sample_InF <- seq(as.Date("2018-01-01"),as.Date("2019-12-30"),by= "weeks")
InF_R <- zoo(T_sample_InF, R_sample_InF)
R_InF = as.ts(InF_R)
R_InF

T_sample_Nasdaq <- dt$NASDAQCOM
R_sample_Nasdaq <- seq(as.Date("2018-01-01"),as.Date("2019-12-30"),by= "weeks")
Nasdaq_R <- zoo(T_sample_Nasdaq, R_sample_Nasdaq)
R_Nasdaq = as.ts(Nasdaq_R)
R_Nasdaq

T_sample_Vix <- dt$VIXCLS
R_sample_Vix <- seq(as.Date("2018-01-01"),as.Date("2019-12-30"),by= "weeks")
Vix_R <- zoo(T_sample_Vix, R_sample_Vix)
R_Vix = as.ts(Vix_R)
R_Vix

##P1 and P2
dtp1 <- read_excel("DATASET.xls", sheet = 2)
head(dtp1)
dtp2 <- read_excel("DATASET.xls", sheet = 3)
head(dtp2)
P_sample_SP500_1 <- dtp1$SP500
P_sample_SP500_2 <- dtp2$SP500
P1_sample_SP500 <- seq(as.Date("2020-01-06"),as.Date("2020-06-29"),by= "weeks")
P2_sample_SP500 <- seq(as.Date("2020-07-06"),as.Date("2021-01-04"),by= "weeks")
Sp500_P1 <- zoo(P_sample_SP500_1, P1_sample_SP500)
Sp500_P2 <- zoo(P_sample_SP500_2, P2_sample_SP500)
P1_SP500 = as.ts(Sp500_P1)
P2_SP500 = as.ts(Sp500_P2)
P1_SP500
P2_SP500

P_sample_DJIA_1 <- dtp1$DJIA
P_sample_DJIA_2 <- dtp2$DJIA
P1_sample <- seq(as.Date("2020-01-06"),as.Date("2020-06-29"),by= "weeks")
P2_sample <- seq(as.Date("2020-07-06"),as.Date("2021-01-04"),by= "weeks")
DJIA_P1 <- zoo(P_sample_DJIA_1, P1_sample)
DJIA_P2 <- zoo(P_sample_DJIA_2, P2_sample)
P1_DJIA = as.ts(DJIA_P1)
P2_DJIA = as.ts(DJIA_P2)
P1_DJIA
P2_DJIA

P_sample_InF_1 <- dtp1$`10 year break inflation rate`
P_sample_InF_2 <- dtp2$`10 year break inflation rate`
P1_sample <- seq(as.Date("2020-01-06"),as.Date("2020-06-29"),by= "weeks")
P2_sample <- seq(as.Date("2020-07-06"),as.Date("2021-01-04"),by= "weeks")
InF_P1 <- zoo(P_sample_InF_1, P1_sample)
InF_P2 <- zoo(P_sample_InF_2, P2_sample)
P1_InF = as.ts(InF_P1)
P2_InF = as.ts(InF_P2)
P1_InF
P2_InF

P_sample_Nasdaq_1 <- dtp1$NASDAQCOM
P_sample_Nasdaq_2 <- dtp2$NASDAQCOM
P1_sample <- seq(as.Date("2020-01-06"),as.Date("2020-06-29"),by= "weeks")
P2_sample <- seq(as.Date("2020-07-06"),as.Date("2021-01-04"),by= "weeks")
Nasdaq_P1 <- zoo(P_sample_Nasdaq_1, P1_sample)
Nasdaq_P2 <- zoo(P_sample_Nasdaq_2, P2_sample)
P1_Nasdaq = as.ts(Nasdaq_P1)
P2_Nasdaq = as.ts(Nasdaq_P2)
P1_Nasdaq
P2_Nasdaq

P_sample_Vix_1 <- dtp1$VIXCLS
P_sample_Vix_2 <- dtp2$VIXCLS
P1_sample <- seq(as.Date("2020-01-06"),as.Date("2020-06-29"),by= "weeks")
P2_sample <- seq(as.Date("2020-07-06"),as.Date("2021-01-04"),by= "weeks")
Vix_P1 <- zoo(P_sample_Vix_1, P1_sample)
Vix_P2 <- zoo(P_sample_Vix_2, P2_sample)
P1_Vix = as.ts(Vix_P1)
P2_Vix = as.ts(Vix_P2)
P1_Vix
P2_Vix
###Benchmark Model to forecast SP500
g.ar2 <- dynlm(R_SP500~L(R_SP500, 1:2))
ar.ols(R_SP500, order.max = 1, demean = F, intercept = T)
ar2g <- ar(R_SP500,h= 1, aic=FALSE, order.max=1, method="ols")
fcst <- data.frame(forecast(ar2g, 3))
forecast_model1 <- forecast(ar2g)
plot(forecast(ar2g,1))
head(dtp1$SP500)
##Forecast error for model 1 h = 1
mse(3242.440, 3230.226)

##Considering h2 = 4
g.ar2 <- dynlm(R_SP500~L(R_SP500, 1:4))
ar.ols(R_SP500, order.max = 4, demean = F, intercept = T)
ar4g <- ar(R_SP500,h= 4, aic=FALSE, order.max=4, method="ols")
fcst <- data.frame(forecast(ar4g, 3))
forecast_model1_h2 <- forecast(ar4g)
forecast_model1_h2
plot(forecast_model1_h2)

##mse when h = h2
valueh2_actual = (3242.440+3263.682+3304.718+3301.436)/4
mse(valueh2_actual,(3228.815+3223.775+3220.841+3219.712)/4)

##Using P2 to forecast h = h1
P2_ar <- dynlm(P2_SP500~L(P2_SP500,1:2))
ar.ols(P2_SP500, order.max = 1, demean = F, intercept = T)
ar4g_P2 <- ar(P2_SP500,h= 1, aic=FALSE, order.max=1, method="ols")
forecast_model1_h1_P2 <- forecast(ar4g_P2)
forecast_model1_h1_P2
plot(forecast_model1_h1_P2)
mse(3780.62,3726.421)

#h = h2
P2_ar_h2 <- dynlm(P2_SP500~L(P2_SP500,1:4))
ar.ols(P2_SP500, order.max = 4, demean = F, intercept = T)
ar4g_P2_h2 <- ar(P2_SP500,h= 4, aic=FALSE, order.max=4, method="ols")
forecast_model1_h2_P2 <- forecast(ar4g_P2_h2)
forecast_model1_h2_P2
plot(forecast_model1_h2_P2)
valueactual_p2 = (3780.62+3793.71+3840.13+3775.17)/4
mse(valueactual_p2,(3736.068+3739.185+3743.514+3750.254)/4)

###Simple Autoregressive Model with Exogenous Variables
Reg <- window(ts.union(R_SP500,R_DJIA,R_InF,R_Nasdaq,R_Vix))
###P = 1 (lag = 1)
Reg_P1 <- VAR(y = Reg, p = 1)
forecast_model2_p1 <- forecast(Reg_P1, h = 1)
forecast_model2_p1
plot(forecast_model2_p1)
summary(forecast_model2_p1)
##forecast model for this model
mse(3242.440,3215.245)

###when h = h2
Reg_P1_h2 <- VAR(y = Reg, p = 4)
forecast_model2_p1_h2 <- forecast(Reg_P1, h = 4)
forecast_model2_p1_h2
plot(forecast_model2_p1_h2)
summary(forecast_model2_p1_h2)
head(dtp1$SP500)

##forecast error for this model
mse(valueh2_actual,(3215.245+3201.620+3190.615+3182.269)/4)

##Simple Autoregressive model using P2 datasample
Reg_P2sample <- window(ts.union(P2_SP500,P2_DJIA,P2_InF,P2_Nasdaq,P2_Vix))
Reg_P2 <- VAR(y = Reg_P2sample, p = 1)
forecast_model2_p2 <- forecast(Reg_P2, h = 1)
forecast_model2_p2
plot(forecast_model2_p1)

##forecast model for this model
mse(3780.62,3739.607)

###When Using P2 data sample and h=h2

Reg_P2_h2 <- VAR(y = Reg_P2sample, p = 1)
forecast_model2_p2_h2 <- forecast(Reg_P2_h2, h = 4)
forecast_model2_p2_h2
plot(forecast_model2_p2_h2)
mse(valueactual_p2,(3739.607+3745.214+3746.014+3742.838)/4)

##P = 2 (lag = 2)
Reg_P2 <- VAR(y = Reg, p = 2)
forecast_model2_p2 <- forecast(Reg_P2, h =1)
forecast_model2_p2
summary(forecast_model2_p2)
##forecast error for this model
mse(3242.440,3215.981)

##P = 3 (lag = 3)
Reg_P3 <- VAR(y = Reg, p = 3)
forecast_model2_p3 <- forecast(Reg_P3)
summary(forecast_model2_p3)

###Althernative Autoregressive type Model _lag = 1
RegForecast <- window((ts.union(R_SP500,R_DJIA,R_Nasdaq)))
RegForecastP1 <- VAR(y = RegForecast, p = 1)
Forecast_result_model3_p1 <- forecast(RegForecastP1,h = 1)
Forecast_result_model3_p1
plot(forecast_model3_p1)

##When h = h2
Forecast_result_model3_p1_h2 <- forecast(RegForecastP1,h = 4)
Forecast_result_model3_p1_h2
plot(Forecast_result_model3_p1_h2)

##forecast error
mse(valueh2_actual,(3235.794+3241.108+3246.932+3253.141)/4)

##Forecast error
mse(3242.440,3235.794)

###Using Alternative Autoregressive with P2 data sample

RegForecast_P2 <- window((ts.union(P2_SP500,P2_DJIA,P2_Nasdaq)))
RegForecastP2 <- VAR(y = RegForecast_P2, p = 1)
Forecast_result_model3_p2 <- forecast(RegForecastP2,h = 1)
Forecast_result_model3_p2
plot(Forecast_result_model3_p2)
mse(3780.62,3740.45)

##When h = h2
Forecast_result_model3_p2_h2 <- forecast(RegForecastP2,h = 4)
Forecast_result_model3_p2_h2
plot(Forecast_result_model3_p2_h2)
mse(valueactual_p2,(3740.450+3748.522+3754.759+3759.936)/4)

##Using h2 = 4 for weekly data
Forecast_result_model3_p1_h4 <- predict(RegForecastP1,p = 1, h = 4)

summary(Forecast_result_model3_p1)

##Regression Analysis
y1 = lm(dt$SP500~dt$DJIA)
summary(y1)

y2 = lm(dt$SP500~dt$DJIA)
summary(y2)

y3 = lm(dt$SP500~dt$`10 year break inflation rate`)
summary(y3)

y4 = lm(dt$SP500~dt$NASDAQCOM)
summary(y4)

y5 = lm(dt$SP500~dt$VIXCLS)
summary(y5)

##Using t test for the P1 sample
t.test(dtp1$SP500)
t.test(dtp1$DJIA)
t.test(dtp1$`10 year break inflation rate`)
t.test(dtp1$NASDAQCOM)
t.test(dtp1$VIXCLS)

###Analyze the SIC of the model using P2 sample
##model1
BIC(lm(dtp2$SP500~dtp2$DJIA))

##model 2
BIC(lm(dtp2$SP500~dtp2$DJIA+dtp2$`10 year break inflation rate`+dtp2$NASDAQCOM+dtp2$VIXCLS))

##model 3
BIC(lm(dtp2$SP500~dtp2$DJIA+dtp2$`10 year break inflation rate`+dtp2$NASDAQCOM))

library(caret)
data(Sacramento)

