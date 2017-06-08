library(MASS)
library(psych)
library(dummies)
library(dplyr)
library(car)

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
# rad has multiple levels. We can group those will lower than 5.2% contribution 
summary(cBoston)
describe(cBoston)
boxplot(cBoston)


# Can treat skew with the following transformations:
#   Positive Skew: log10(1+x), Negative Skew: -log10(1+abs(x))
for(i in c(1,2,3,4,5,7,9,12)) {
  cBoston[,i] <- log10(1+cBoston[,i])
}

for(i in c(6,10,11)) {
  cBoston[,i] <- -log10(1+abs(cBoston[,i]))
}
describe(cBoston)
boxplot(cBoston)


# group rad where propotion is less than 5.2%
cBoston$rad <- as.factor(cBoston$rad)
groupByRad <- group_by(cBoston, rad) %>%
  summarise(f = n()/size) %>%
  filter(f <= 0.052)

groupByRad <- data.frame(groupByRad)
levels(cBoston$rad)[levels(cBoston$rad) %in% groupByRad$rad] <- "Other"

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
round(cor(train[,-c(8,13)]),2)

View(train)

# Some of the variables are highly significant and rad.24 needs to be omitted as it is linearly realted to the other variables
lm <- lm(train$medv~., train)
summary(lm)
# Ideally we can remove variables one by one by using variable inflation factor but we shall use PCA & Factor Analysis for the same
vif(lm)

# Predict using the linear regression equation
pMedv_lm <- predict(lm, test)

# RMSE - Root mean square error to determine the accuracy of the model
rmseLMwithTrain <- sqrt( sum( ((lm$fitted.values - train$medv)^2) ) / nrow(train) )
rmseLMwithTest <- sqrt( sum( ((pMedv_lm - test$medv)^2) ) / nrow(test) )

# 5 PCs explain 84.5% of the variation and including those which have variances > 0.6
# We shall not include the dummy variables to determine the PC's
names(train)
pcs <- prcomp(train[,-c(12,13)], scale = FALSE)
summary(pcs)
plot(pcs, type="l", main="Scree Plot", col="blue")

# Factor Analysis
noRotation <- principal(train[,-c(12,13)], nfactors=5, rotate="none")
noRotation

# Increasing the factors to six as the grouping does not seem right Crim & Tax together does not seem right
# RC1 - City Center
# - Dis: weighted mean of distances to five Boston employment centres. 
# + Indus: proportion of non-retail business acres per town.
# + Nox: nitrogen oxides concentration (parts per 10 million).
# + Age: proportion of owner-occupied units built prior to 1940.
# + Tax: Tax: full-value property-tax rate per \$10,000.

# RC2 - Small Dwellings
# - Rm: average number of rooms per dwelling.
# + Lstat: lower status of the population (percent).

# Indivisual factors for each of them
# Zn: proportion of residential land zoned for lots over 25,000 sq.ft. 
# Crim: per capita crime rate by town.
# Ptratio: pupil-teacher ratio by town.
# Black: 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town. 

# Medv: median value of owner-occupied homes in $1000s.
# Rad: index of accessibility to national highway
# Chas: Charles River dummy variable (= 1 if tract bounds river; 0 otherwise). 

# Le's use factory analysis with varimax rotation to understand which dimension can be grouped to-gether
variMaxRotation <- principal(train[,-c(12,13)], nfactors=6, rotate="varimax")
variMaxRotation
round(cov(variMaxRotation$scores),2)

names(train)
train <- data.frame(train, variMaxRotation$scores)

# Need to for the test data - nTest <- data.frame(predict(pcs, newdata = test)[,1:7])
colnames(train)
fLM <- lm(train$medv~., train[,c(12:19)])
summary(fLM)

# Prepare the test data with Factor Weights
colNames <- colnames(variMaxRotation$weights)
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
pMedv_flm <- predict(fLM, test[,c(13:19)])

rmseFactorswithTrain <- sqrt( sum( ((fLM$fitted.values - train$medv)^2) ) / nrow(train) )
rmseFactorswithTest <- sqrt( sum( ((pMedv_flm - test$medv)^2) ) / nrow(test) )

"R^2 for linear regression and linear regression by using factor"
summary(lm)
summary(fLM)

"RMSE without PC:"
rmseLMwithTrain
rmseLMwithTest

"RMSE with Facors:"
rmseFactorswithTrain
rmseFactorswithTest
