# read the data to determine if the customer would avail the term deposit 
# https://archive.ics.uci.edu/ml/datasets/Bank+Marketing
train <- read.csv("./data/bank.csv", sep = ";")
test <- read.csv("./data/bank-full.csv", sep = ";")
train$target <- ifelse(train$y=="yes",1,0)
test$target <- ifelse(test$y=="yes",1,0)
head(train)

#remove the column - y - that is the result
train <- train[,-17]
test <- test[-17]

library("dplyr")

train$year <- 2012

train$month <- ifelse(train$month == "jan", "01", 
                      ifelse(train$month == "feb", "02", 
                           ifelse(train$month == "mar","03",
                                  ifelse(train$month == "apr","04",
                                         ifelse(train$month == "may","05",
                                                ifelse(train$month == "jun","06",
                                                       ifelse(train$month == "jul","07",
                                                              ifelse(train$month == "aug","08",
                                                                     ifelse(train$month == "sep","09",
                                                                            ifelse(train$month == "oct","10",
                                                                                   ifelse(train$month == "nov","11","12")))))))))))
library("tidyr")
train <- unite(train, date, year, month, day, sep = "-")
head(train)
train$lastContacted <- as.double(difftime(Sys.Date(), as.Date(train$date), units = "days"))
head(train)
train <- train[,-10]
head(train)
sTrain <- data.frame(scale(train[,c(1,6,10,11,12,13,16)], center = TRUE, scale = TRUE))
temp <- train[,-c(1,6,10,11,12,13,16)]
train <- bind_cols(temp, sTrain)
head(train)

# response rate of the training data set & determine the bucket size and the minsplit
responseRate <- sum(train$target)/nrow(train) 
responseRate

sizeOfTheDataSet <- nrow(train)
sizeOfTheDataSet

minBucketSize <- 1*sizeOfTheDataSet/100 
minBucketSize

minSplitSize <- 3*minBucketSize  
minSplitSize

# specifying the criteria - minBucket: 2-3% of pupulation, minSplit: 3*minBucket
library("rpart")
controlCriteria = rpart.control(minsplit=minSplitSize, minbucket = minBucketSize, cp = 0, xval = 10)

# build the decison tree based on CART
classificationModel <- rpart(formula = target ~ ., data = train, method = "class", control = controlCriteria)
classificationModel
library("rpart.plot")
library("rattle")
fancyRpartPlot(classificationModel)

# use the CP value where the xerror is the least
printcp(classificationModel)
plotcp(classificationModel)


# prune the tree not sure what to do
classificationModel <- prune(classificationModel, cp = 0.002 ,"CP")
printcp(classificationModel)
fancyRpartPlot(classificationModel)

# predict the target for the development sample
train$pTarget <- predict(classificationModel, train, type="class")
predictionScore <- predict(classificationModel, train)
train$pPositiveScore <- predictionScore[,2]

" 
Scoring the model based on the development sample
lift chart to determine model performance
Need help on how to draw charts: https://www.analyticsvidhya.com/blog/2016/02/7-important-model-evaluation-error-metrics/
"
train <- mutate(train, deciles = ntile(pPositiveScore, 10))
sampleSize <- nrow(train)  
responseRate <- (sum(train$target)/sampleSize)*100
noOfResponseForResponders <- sum(train$target==1)
noOfResponseForNonResponders <- sum(train$target==0)
trainByDecile <- group_by(train, deciles)
trainByDecile <- summarise(trainByDecile, 
                               n = n(), 
                               resonders = sum(target==1), 
                               nonResponders = sum(target==0), 
                               pResponders = sum(pTarget==1),
                               pNonResponders = sum(pTarget==0),   
                               responseRateForNonResponders = (nonResponders/noOfResponseForNonResponders)*100,
                               responseRateForResponders = (resonders/noOfResponseForResponders)*100
)
trainByDecile <- arrange(trainByDecile, desc(deciles))
trainByDecile <- mutate(trainByDecile, 
                            cumResponseRateForResponders = cumsum(responseRateForResponders),
                            cumResponseRateForNonResponders = cumsum(responseRateForNonResponders),
                            perPopulation = (n/sampleSize)*100,
                            cumPerPopulation = cumsum(perPopulation),
                            liftAtDecile = (responseRateForResponders/perPopulation)*100,
                            totalLift = (cumResponseRateForResponders/cumPerPopulation)*100,
                            ks = (cumResponseRateForResponders - cumResponseRateForNonResponders)
)
trainByDecile$newDeclie <- 11 - trainByDecile$deciles
View(trainByDecile)

plot(0,100,xlim = c(10,100),ylim = c(10,100),type = "n")
abline(coef=c(0,1), col = "red")
lines(trainByDecile$cumPerPopulation, trainByDecile$cumResponseRateForResponders, col = "blue")

library(ggplot2)
ggplot(data = trainByDecile) + 
  scale_y_continuous() +
  scale_x_continuous(breaks = c(10,20,30,40,50,60,70,80,90,100)) +
  geom_abline(intercept = 0) + 
  geom_line(aes(trainByDecile$cumPerPopulation, trainByDecile$cumResponseRateForResponders), colour = "blue")

ggplot(data = trainByDecile) + 
  geom_line(aes(trainByDecile$deciles, trainByDecile$totalLift), colour = "blue") + 
  scale_x_continuous(breaks = c(1:10)) + geom_hline(yintercept = 100)

"
confusion matrix to determine model performance
https://www.analyticsvidhya.com/blog/2016/02/7-important-model-evaluation-error-metrics/
"
library("caret")
confusionMatrix(train$pTarget, train$target)

"
Kolomogorov Smirnov chart
"
plot(0,100,xlim = c(1,10),ylim = c(10,100),type = "n")
lines(trainByDecile$deciles, trainByDecile$cumResponseRateForResponders, col = "red")
lines(trainByDecile$deciles, trainByDecile$cumResponseRateForNonResponders, col = "blue")

"
Other methods to determine model performance
ROC curve on the other hand is almost independent of the response rate
AUC - Area under curve
Gini Coffecient -
"
library("ROCR")
predition <- prediction(train$pPositiveScore, train$target)
tprFpr <- performance(predition, "tpr", "fpr")
plot(tprFpr)
KS <- max(attr(tprFpr, 'y.values')[[1]]-attr(tprFpr, 'x.values')[[1]])
auc <- performance(predition,"auc"); 
auc <- as.numeric(auc@y.values)
giniCoffecient <- (2*auc)-1

# scale and determine 
test$year <- 2012

test$month <- ifelse(test$month == "jan", "01", 
                      ifelse(test$month == "feb", "02", 
                             ifelse(test$month == "mar","03",
                                    ifelse(test$month == "apr","04",
                                           ifelse(test$month == "may","05",
                                                  ifelse(test$month == "jun","06",
                                                         ifelse(test$month == "jul","07",
                                                                ifelse(test$month == "aug","08",
                                                                       ifelse(test$month == "sep","09",
                                                                              ifelse(test$month == "oct","10",
                                                                                     ifelse(test$month == "nov","11","12")))))))))))
test <- unite(test, date, year, month, day, sep = "-")
head(test)
test$lastContacted <- as.double(difftime(Sys.Date(), as.Date(test$date), units = "days"))
head(test)
test <- test[,-10]
head(test)
sTest <- data.frame(scale(test[,c(1,6,10,11,12,13,16)], center = TRUE, scale = TRUE))
temp <- test[,-c(1,6,10,11,12,13,16)]
test <- bind_cols(temp, sTest)
head(test)


# predict the target for the test sample
test$pTarget <- predict(classificationModel, test, type="class")
predictionScore <- predict(classificationModel, test)
test$pPositiveScore <- predictionScore[,2]

"
confusion matrix to determine model performance
https://www.analyticsvidhya.com/blog/2016/02/7-important-model-evaluation-error-metrics/
"
confusionMatrix(train$pTarget, train$target)
confusionMatrix(test$pTarget, test$target)

" 
Scoring the model based on the development sample
lift chart to determine model performance
Need help on how to draw charts: https://www.analyticsvidhya.com/blog/2016/02/7-important-model-evaluation-error-metrics/
"
library("dplyr")
test <- mutate(test, deciles = ntile(pPositiveScore, 10))
sampleSize <- nrow(test)  
responseRate <- (sum(test$target)/sampleSize)*100
noOfResponseForResponders <- sum(test$target==1)
noOfResponseForNonResponders <- sum(test$target==0)
head(test)
testByDecile <- group_by(test, deciles)
testByDecile <- summarise(testByDecile, 
                                   n = n(), 
                                   resonders = sum(target==1), 
                                   nonResponders = sum(target==0), 
                                   pResponders = sum(pTarget==1),
                                   pNonResponders = sum(pTarget==0),   
                                   responseRateForNonResponders = (nonResponders/noOfResponseForNonResponders)*100,
                                   responseRateForResponders = (resonders/noOfResponseForResponders)*100
)
testByDecile <- arrange(testByDecile, desc(deciles))
testByDecile <- mutate(testByDecile, 
                                cumResponseRateForResponders = cumsum(responseRateForResponders),
                                cumResponseRateForNonResponders = cumsum(responseRateForNonResponders),
                                perPopulation = (n/sampleSize)*100,
                                cumPerPopulation = cumsum(perPopulation),
                                liftAtDecile = (responseRateForResponders/perPopulation)*100,
                                totalLift = (cumResponseRateForResponders/cumPerPopulation)*100,
                                ks = (cumResponseRateForResponders - cumResponseRateForNonResponders)
)
View(testByDecile)

"
Other methods to determine model performance
ROC curve on the other hand is almost independent of the response rate
AUC - Area under curve
Gini Coffecient -
"
hPredition <- prediction(test$pPositiveScore, test$target)
hTprFpr <- performance(hPredition, "tpr", "fpr")
plot(hTprFpr)
hKS <- max(attr(hTprFpr, 'y.values')[[1]]-attr(hTprFpr, 'x.values')[[1]])
hAuc <- performance(hPredition,"auc"); 
hAuc <- as.numeric(hAuc@y.values)
hGiniCoffecient <- (2*hAuc)-1

"
Comparing train & test
"

KS
hKS

auc
hAuc

giniCoffecient
hGiniCoffecient

