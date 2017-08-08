library(tseries)
library(forecast)
library(smooth)
library(astsa)

all <- read.csv("./glAssignmnets/Forecast Sales - Time Series/Sales Data for TS Assgn.csv")
head(all)
tail(all)

# Convert to a timeseries object explicitly stating start and end points
all_ts <- ts(all[,-c(1,2)], start=c(2011,1), end=c(2017,5), frequency=12)
# There is a trend, definite sesonality, no long runs, the series is not stationary
plot(all_ts)

# Split in test & train
train <- window(all_ts, start=c(2011,1), end=c(2015,12))
test <- window(all_ts, start=c(2016,1), end=c(2017,5))

# Function to calculate MAPE
mape <- function(actual, predicted) {
  meanAbsoluteError <- mean(abs(actual - predicted)/actual)
  return(meanAbsoluteError)
}

# Let us decompose the series into the following components - trend, sesonality and random & plot
all_decomposed <- decompose(all_ts, type = "multiplicative")
plot(all_decomposed)
meanOfRandom <- mean(na.omit(all_decomposed$random))
# The random part is stationary as the p-value is < 0.05
adf.test(na.omit(all_decomposed$random))

# Forecast using moving average - We use moving average of 12 periods as there is sesonality of the order of 12 
ma_all <- ma(all_ts, order = 12)
ts.plot(all_ts, ma_all, col=c('red', 'forest green'))
ma_forecast_test <- sma(all_ts, order=12, h=17, holdout=TRUE) 
ma_forecast <- sma(all_ts, order=12, h=19, holdout=FALSE) 

# Checking the forecast accuracy for test
forecasted <- ma_forecast_test$forecast * all_decomposed$seasonal * meanOfRandom
ts.plot(test, forecasted, col=c('red', 'forest green'))
mape.fit1 <- mape(test, forecasted)
mape.fit1

# Forecast from June 2017 to Dec 2018
seasonJuneToDec <- window(all_decomposed$seasonal, start=c(2015,6), end=c(2016,12))
seasonJuneToDec <- data.frame(seasonJuneToDec)
seasonJuneToDec <- ts(seasonJuneToDec, start=c(2017,6), end=c(2018,12),frequency = 12)  
forecastDec2018 <- ma_forecast$forecast * seasonJuneToDec * meanOfRandom
forecastDec2018
ts.plot(all_ts, forecastDec2018, col=c('red', 'forest green'))

#free up memory
rm(all_decomposed, ma_all, all, ma_forecast, ma_forecast_test, 
   seasonJuneToDec, forecastDec2018, forecasted)

# Let us use hotWinters approach to forecast
fit2 <- HoltWinters(train, l.start = 0.2, b.start = 0.2, gamma = 0.4, seasonal = "multiplicative")
plot(fit2)
fit2
fit2.forecastForTest <- forecast(fit2,17)
ts.plot(test, fit2.forecastForTest$mean,col=c('red', 'forest green'))
mape.fit2 <- mape(test, fit2.forecastForTest$mean)
mape.fit2

fit2 <- HoltWinters(all_ts, alpha = fit2$alpha, beta = fit2$beta, gamma = fit2$gamma, seasonal = "multiplicative")
plot(fit2)
fit2.forecast <- forecast(fit2,19)
ts.plot(all_ts, fit2.forecast$mean,col=c('red', 'forest green'))

# The random part is stationary as the p-value is < 0.05
adf.test(na.omit(fit2.forecast$residuals))
plot(fit2.forecast$residuals)

#free up memory
rm(all_ts, fit2, fit2.forecastForTest, test, train, mape)
