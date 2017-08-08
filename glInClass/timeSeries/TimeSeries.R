library(smooth)
library(forecast)
library(graphics)
library(datasets)
library(tseries)


# Import Bond_Yield data #
Bond_Yield <- read.csv("./glInClass/timeSeries/BondYield.csv")
head(Bond_Yield)
# Convert to a timeseries object explicitly stating start and end points and declaring yearly data #
BY <- ts(Bond_Yield[,-1], start=c(1900,1), end=c(1970,1), frequency=1)
plot(BY)

# Moving average with different periods #
BY3 <- ma(BY, order=3) #Library{forecast}
BY9 <- ma(BY, order=9)
BY19 <- ma(BY, order=19)

ts.plot(BY, BY3, BY9, BY19, lty=c(1:4), col=c('black','red','dark blue', 'forest green'))

#################################
ChampagneCalc <- read.csv("./glInClass/timeSeries/Champagne.csv")
Champagne <- ts(ChampagneCalc[,-1], start=c(1964,1), end=c(1972,9), frequency=12)
plot(Champagne, col='dark blue', main="Sales (in millions)")

# Load AirPassengers data from Library{datasets}#
AirPassengers <- ts(AirPassengers, start=c(1949, 1), end=c(1960, 12), frequency=12)
plot(AirPassengers)

# Decompose time series into additive components #
Champ.add <- decompose(Champagne, type="additive")
plot(Champ.add$figure, type="l", xlab="Month", ylab="", 
     main="Champagne Sales")  # Plots seasonality indices
plot(Champ.add$trend, type="l") # Plots trend

# Decompose time series into multiplicative components #
AirP.mult <- decompose(AirPassengers, type="multiplicative")
plot(AirP.mult$figure, type="l") # Plots seasonality indices
plot(AirP.mult$trend, type="l") # Plots trend
plot(AirP.mult)


############################################
# Forecasting using MA
BY3H <- sma(BY, order=3, h=2, holdout=T) #Library{smooth}
BY19H <- sma(BY, order=9, h=2, holdout=T)

# Exponential Smoothing
BYfit2 <- HoltWinters(BY, alpha=0.2, beta=FALSE, gamma=FALSE)
plot(BYfit2)
BYfit5 <- HoltWinters(BY, alpha=0.5, beta=FALSE, gamma=FALSE)
plot(BYfit5)
BYfit8 <- HoltWinters(BY, alpha=0.8, beta=FALSE, gamma=FALSE)
plot(BYfit8)
ts.plot(BY, BYfit2$fitted, BYfit5$fitted, lty=c(1:3), col=c('black','red',' dark blue'))

BYfit2P <- predict(BYfit2, n.ahead=10, prediction.interval = TRUE, level=0.95)

# Exponential smoothing #
Champ.fit1ex1 <- HoltWinters(Champagne, alpha=0.2, beta=F, gamma=0.4, seasonal = "additive")
plot(Champ.fit1ex1, main="Holt-Winters Filtering: Champagne Sales")

Champ.fit1ex2 <- HoltWinters(Champagne, alpha=0.9, beta=F, gamma=F)
plot(Champ.fit1ex2, main="SES (alpha = 0.9): Champagne Sales")

Champ.fit3ex1 <- HoltWinters(Champagne, alpha=0.3, beta=0.1, gamma=0.7, seasonal = "add")
plot(Champ.fit3ex1, main="HW (alpha = 0.3, beta=0.1, gamma=0.7): Champagne Sales")

# simple exponential - models level
fit1 <- HoltWinters(AirPassengers, alpha=0.5, beta=FALSE, gamma=FALSE, seasonal = "mult")
plot(fit1)

# double exponential - models level and trend
fit2 <- HoltWinters(AirPassengers, alpha=0.5, beta=0.5, gamma=FALSE)
plot(fit2)

# triple exponential - models level, trend, and seasonal components
fit3 <- HoltWinters(AirPassengers, alpha=0.3, beta=0.3, gamma=0.9, seasonal = "mult")
plot(fit3, main="HW(alpha=0.3, beta=0.9, gamma=0.9)")


# ..............................................  #
# Play with alpha, beta, gamma combination 
# to see marked difference in fit and prediction #
# ............................................... #

# predictive accuracy #
library(forecast)

########### Champagne  #######
Champ1 <- window(Champagne, start=c(1964,1), end=c(1970,12))
ChampHO <- window(Champagne, start=c(1971,1), end=c(1972,9))
plot(Champ1)

Champ1.fit3ex1 <- HoltWinters(Champ1, alpha=0.3, beta=0.1, gamma=0.7, seasonal = "add")
Champ1Exp3 <- forecast(Champ1.fit3ex1, 21)
plot(Champ1Exp3)

Champ1Exp3 <- forecast(Champ1.fit3ex1, 21)
plot(Champ1Exp3)

Vec1<- cbind(ChampHO,Champ1Exp3$mean)
ts.plot(Vec1, col=c("blue", "red"))
MAPE <- mean(abs(Vec1[,1]-Vec1[,2])/Vec1[,1])
MAPE

###########################################
AirPax1 <- window(AirPassengers, start=c(1949,1), end=c(1959,12))
AirPaxHO <- window(AirPassengers, start=c(1960,1), end=c(1960,12))
# predict next 12 future values #
AirPax1fit3 <- HoltWinters(AirPax1, alpha=0.3, beta=0.3, gamma=0.9, seasonal = "mult")
plot(AirPax1fit3, main="HW(alpha=0.3, beta=0.9, gamma=0.9)")

AirPaxFC <- forecast(AirPax1fit3, 12)
plot(AirPaxFC)

Vec2<- cbind(AirPaxHO,AirPaxFC$mean)
ts.plot(Vec2, col=c("blue", "red"))
MAPE <- mean(abs(Vec2[,1]-Vec2[,2])/Vec2[,1])
MAPE

###############################################
# Autocorrelation #

BY1 <- lag(BY, k=-1)
df1 <- diff(BY, order=1)
cbind(BY, BY1, diff(BY, order=1))

AC1 <- acf(BY, lag.max=25) # Autocorrelation 
AC1
PAC1 <- pacf(BY, lag.max=25) # Partial autocorrelation
PAC1
####################
# Test for Stationarity
adf.test(na.omit(Champ.add$random)) # library(tseries)
adf.test(na.omit(AirP$random))

######################
# Differencing
Clay_Bricks <- read.csv("./glInClass/timeSeries/Clay Bricks.csv")
Clay <- ts(Clay_Bricks[,-1], start=c(1956,1), end=c(1995,8), frequency=12)
plot(Clay, col="blue")
adf.test(Clay)

Clay1 <- lag(Clay, k=-1)
Claydf1<- diff(Clay) # Lag 1 difference

Clay2 <- lag(Clay, k=-2)
Claydf2<- diff(Clay, lag=2) # Lag 2 difference

plot(Claydf1, col="purple", main="First Difference: Clay Brick")
adf.test(Claydf1)

acf(Clay, main="Clay Brick")
acf(Claydf1, main="First Difference: Clay Brick")

########## BASF ##########
German_Stock <- read.csv("./glInClass/timeSeries/GermanMonthlyAverageStockPrice.csv")
BASF <- ts(German_Stock[,9], start=c(1981,1), end=c(1993,12), frequency=12)
plot(BASF, col="dark green")
acf(BASF, lag=30, main="BASF Monthly Average")
BASF1 <- diff(BASF, order=1)
acf(BASF1, lag=30, main="BASF First Difference")
pacf(BASF, lag=30, main="BASF Monthly Average")
pacf(BASF, lag=30, main="BASF First Difference")

BASF.arima.fit <- arima(BASF, c(1, 1, 0))
Box.test(BASF.arima.fit$residuals, lag = 60, type = "Ljung-Box")


##### BMW ######
BMW <- ts(German_Stock[,4], start=c(1981,1), end=c(1993,12), frequency=12)
plot(BMW, col="blue")
acf(BMW, lag=100)
pacf(BMW, lag=100)

BMW1 <- diff(BMW, order=1)
plot(BMW1, ylab="First Difference: BMW", col="brown")
acf(BMW1, lag=50)
pacf(BMW1, lag=50)

adf.test(BMW)
adf.test(BMW1)

