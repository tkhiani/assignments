library(tseries)
library(forecast)
library(smooth)
library(LSTS)
library(astsa)
library(datasets)

Bond_Yield <- read.csv("./glInClass/timeSeries/BondYield.csv")
head(Bond_Yield)
tail(Bond_Yield)
# Convert to a timeseries object explicitly stating start and end points and declaring yearly data #
BY <- ts(Bond_Yield[,-1], start=c(1900,1), end=c(1970,1), frequency=1)
plot(BY)

ts.plot(BY, ma(BY, order = 3), ma(BY, order = 9), col=c('black','red','dark blue', 'forest green'))
BY3H <- sma(BY, order=3, h=3, holdout=T) 
BY19H <- sma(BY, order=9, h=9, holdout=T)

BY_T <- window(BY, start=c(1900,1), end=c(1966,1))
BY_H <- window(BY, start=c(1967,1), end=c(1970,1))
BY_HW.fit <- HoltWinters(BY_T, alpha=0.8, beta=1, gamma=FALSE)
plot(BY_HW.fit)
BY_HO_P <- forecast(BY_HW.fit, 4)
plot(BY_HO_P)
ts.plot(BY_HO_P, BY_H)
mape_HW <- mean(abs(BY_H - BY_HO_P$mean)/BY_H)
mape_HW

acf2(BY_T)
acf2(diff(BY_T, 1))
adf.test(BY_T)
BY_A.fit <- arima(BY_T, order = c(1,0,0))
BY_A.fit
BY_ARIMA_P <- forecast(BY_A.fit, 4)
plot(BY_ARIMA_P)
mape_ARIMA <- mean(abs(BY_H - BY_ARIMA_P)/BY_H)
mape_ARIMA


AirPassengers <- ts(AirPassengers, start=c(1949, 1), end=c(1960, 12), frequency=12)
ap_t <- window(AirPassengers, start=c(1949,1), end=c(1958,12))
ap_h <- window(AirPassengers, start=c(1959,1), end=c(1960,12))

plot(AirPassengers)
ts.plot(AirPassengers, ma(AirPassengers, order = 12), col=c('black','red'))
# Not a stationary series even after accounting for trend
plot(AirPassengers - ma(AirPassengers, order = 12))
# Decompose
decomposed_airPassenders <- decompose(AirPassengers, type = "multiplicative")
adf.test(na.omit(decomposed_airPassenders$random))
Box.Ljung.Test(decomposed_airPassenders$random, lag = 48)
plot(decomposed_airPassenders)

acf2(diff(AirPassengers, 1))
f_ <- forecast(arima(ap_t, c(2,1,1), seasonal = c(2,1,1)), 24)
ts.plot(ap_h, f_$mean, col = c("red","blue"))
mean(abs(ap_h - f_$mean)/ap_h)
Box.Ljung.Test(f_$residuals, lag = 48)
Box.Ljung.Test(AirPassengers, lag = 48)
adf.test(AirPassengers)
adf.test(f_$residuals)
