library(MASS)
library(psych)
library(dplyr)
library(car)
library(corrplot)
library(ggplot2)
library(reshape2)

# Exploratory Data Analysis
head(Boston)
dim(Boston)
str(Boston)

# Take a copy of the Boston data set and start to manipuate it
cBoston <- Boston
size <- nrow(cBoston)
# ignore chas as stated by the 
cBoston <- cBoston[,-4]

# Crim, Zn, Indus, nox, rm, dis, tax, lstat are positively skewed
# age, ptratio, black are negatively skewed
# Outliers exists in crim, zn, chas, black
summary(cBoston)
describe(cBoston)
multi.hist(cBoston)
boxplot(cBoston)

# Can treat skew with the following transformations:
#   Positive Skew: log10(1+x), Negative Skew: -log10(1+abs(x))
for(i in c(1,2,3,4,5,7,9,12)) {
  cBoston[,i] <- log10(1+cBoston[,i])
}

for(i in c(6,10)) {
  cBoston[,i] <- -log10(1+abs(cBoston[,i]))
}
describe(cBoston)
multi.hist(cBoston)

# We can scale the data as the variances are widely different
names(cBoston)
var(cBoston[,-c(8, 13)])
medv <- cBoston$medv
rad <- cBoston$rad
cBoston <- data.frame(scale(cBoston[,-c(8, 13)]))
cBoston$medv <- medv
cBoston$rad <- rad

# lets split the data into train & test
set.seed(1000)
indexes = sample(size,450) 
train = cBoston[indexes,]
test = cBoston[-indexes,]

# Removing dataset from memory
rm(cBoston)

# Variables are correlated
# Justify for a dimension reduction technique
names(train)
r <- round(cor(train[,-c(8,13)]),2)
corrplot(r, order = "hclust", tl.col='black', tl.cex=.75) 


# Some of the variables are highly significant
# age & indus can be omitted from linear regression
lm <- lm(train$medv~., train)
summary(lm)
# Ideally we can remove variables one by one by using variable inflation factor but we shall use PCA & Factor Analysis for the same
vif(lm)

# Predict using the linear regression equation
pMedv_lm <- predict(lm, test, interval = "confidence")

# RMSE - Root mean square error to determine the accuracy of the model
rmseLMwithTrain <- sqrt( sum( ((lm$fitted.values - train$medv)^2) ) / nrow(train) )
rmseLMwithTest <- sqrt( sum( ((pMedv_lm[,1] - test$medv)^2) ) / nrow(test) )

rmseLMwithTest

# 3 PCs explain 75% of the variation and including those which have variances > 1
names(train)
pcs <- prcomp(train[,-c(12,13)], scale = FALSE)
summary(pcs)
pcs$rotation
plot(pcs, type="l", main="Scree Plot", col="blue")

# Factor Analysis
noRotation <- principal(train[,-c(12,13)], nfactors=4, rotate="none")
noRotation

# Increasing the factors to five as the grouping does not seem right and we would like to cover at least 87% of the variation
# Factor 1: City Outskirts (RC1)
# - indus: proportion of non-retail business acres per town
# - nox: nitrogen oxides concentration (parts per 10 million).
# + zn: proportion of residential land zoned for lots over 25,000 sq.ft.
# + age: proportion of owner-occupied units built prior to 1940
# + dis: weighted mean of distances to five Boston employment centres

# Factor 2: High Alert Zone (RC5)
# crim: per capita crime rate by town
# tax: full-value property-tax rate per \$10,000

# Factor 3: Small Dwellings (RC2)
# rm: average number of rooms per dwelling
# lstat: lower status of the population (percent)

# Factor 4: High Pupil-Teacher Ratio (RC4)
# ptratio: pupil-teacher ratio by town

# Factor 5: High Black Population (RC3)
# Black: 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town

# Le's use factory analysis with varimax rotation to understand which dimension can be grouped to-gether
variMaxRotation <- principal(train[,-c(12,13)], nfactors=5, rotate="varimax")
variMaxRotation
round(cov(variMaxRotation$scores),2)

names(train)
train <- data.frame(train, variMaxRotation$scores)

# create model based on factors
colnames(train)
fLM <- lm(train$medv~., train[,c(12:18)])
summary(fLM)

# Prepare the test data with Factor Weights
colNames <- colnames(variMaxRotation$weights)
colNames
for (i in colNames) {
  test[,i] <- variMaxRotation$weights[1,i] * test$crim +
    variMaxRotation$weights[2,i] * test$zn +
    variMaxRotation$weights[3,i] * test$indus +
    variMaxRotation$weights[4,i] * test$nox +
    variMaxRotation$weights[5,i] * test$rm +
    variMaxRotation$weights[6,i] * test$age +
    variMaxRotation$weights[7,i] * test$dis +
    variMaxRotation$weights[8,i] * test$tax +
    variMaxRotation$weights[9,i] * test$ptratio +
    variMaxRotation$weights[10,i] * test$black +
    variMaxRotation$weights[11,i] * test$lstat
}
names(test)
pMedv_flm <- predict(fLM, test[,c(13:18)],interval = "confidence")

rmseFactorswithTrain <- sqrt( sum( ((fLM$fitted.values - train$medv)^2) ) / nrow(train) )
rmseFactorswithTest <- sqrt( sum( ((pMedv_flm[,1] - test$medv)^2) ) / nrow(test) )

"R^2 for linear regression and linear regression by using factor"
summary(lm)
summary(fLM)

"RMSE without PC:"
rmseLMwithTrain
rmseLMwithTest

"RMSE with Facors:"
rmseFactorswithTrain
rmseFactorswithTest

actual.predicted.test <- data.frame(medv = test$medv, 
                                         pMedv.f = pMedv_flm[,1],
                                         pMedv.wf = pMedv_lm[,1],
                                         pMedv_lw = pMedv_flm[,2], 
                                         pMedv_up = pMedv_flm[,3]) 

# Compare the actual vs predicted with factors and without factors
ggplot(data = actual.predicted.test) + 
  geom_line(aes(x = row_number(medv), y = medv), colour = "Red") +
  geom_line(aes(x = row_number(pMedv.f), y = pMedv.f), colour = "Blue") +
  geom_line(aes(x = row_number(pMedv.wf), y = pMedv.wf), colour = "Light Blue") +
  scale_x_continuous(breaks = seq(0,200, 5)) + 
  scale_y_continuous(breaks = seq(0,100, 5))

# Confidence Interval of actual vs predicted (fit, upper and lower limit)
ggplot(data = actual.predicted.test) + 
  geom_line(aes(x = row_number(medv), y = medv), colour = "Red") +
  geom_line(aes(x = row_number(pMedv.f), y = pMedv.f), colour = "Blue") +
  geom_point(aes(x = row_number(pMedv_lw), y = pMedv_lw), colour = "Dark Blue") +
  geom_point(aes(x = row_number(pMedv_up), y = pMedv_up), colour = "Dark Blue") +
  scale_x_continuous(breaks = seq(0,100, 10)) +
  scale_y_continuous(breaks = seq(0,100, 5))


