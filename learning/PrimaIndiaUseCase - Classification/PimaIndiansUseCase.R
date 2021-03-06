# read the prima indians data
diabateseData <- read.csv("./learning/PrimaIndiaUseCase - Classification/pima-indians-data.csv")
head(diabateseData)
# split the data into train and test
library(dplyr)
train <- sample_frac(diabateseData, 0.7)
test <- diabateseData[-as.numeric(rownames(train)),]

# response rate of the training data set
responseRate <- sum(train$target)/nrow(train) 

# specifying the criteria - minBucket: 2-3% of pupulation, minSplit: 3*minBucket
library("rpart")
controlCriteria = rpart.control(minsplit=45, minbucket = 15, cp = 0, xval = 10)

# build the decison tree based on CART
classificationModel <- rpart(formula = target ~ ., data = train, method = "class", control = controlCriteria)
classificationModel
library("rpart.plot")
library("rattle")
fancyRpartPlot(classificationModel)

# use the CP value where the xerror is the least
printcp(classificationModel)
plotcp(classificationModel)


# prune the tree to avoid overfitting CP - 0.0192308
classificationModel <- prune(classificationModel, cp = 0.02 ,"CP")
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
View(train)
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
View(trainByDecile)

"
confusion matrix to determine model performance
https://www.analyticsvidhya.com/blog/2016/02/7-important-model-evaluation-error-metrics/
"
library("caret")
confusionMatrix(train$pTarget, train$target)

"
Kolomogorov Smirnov chart
"
plot(trainByDecile$deciles, trainByDecile$ks, type = "l")

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

# predict the target for the test sample
test$pTarget <- predict(classificationModel, test, type="class")
predictionScore <- predict(classificationModel, test)
test$pPositiveScore <- predictionScore[,2]

" 
Scoring the model based on the development sample
lift chart to determine model performance
Need help on how to draw charts: https://www.analyticsvidhya.com/blog/2016/02/7-important-model-evaluation-error-metrics/
"
test <- mutate(test, deciles = ntile(pPositiveScore, 10))
View(test)
sampleSize <- nrow(test)  
responseRate <- (sum(test$target)/sampleSize)*100
noOfResponseForResponders <- sum(test$target==1)
noOfResponseForNonResponders <- sum(test$target==0)
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
confusion matrix to determine model performance
https://www.analyticsvidhya.com/blog/2016/02/7-important-model-evaluation-error-metrics/
"
confusionMatrix(train$pTarget, train$target)
confusionMatrix(test$pTarget, test$target)

"
Comparing dev & hold out
"

KS
hKS

auc
hAuc

giniCoffecient
hGiniCoffecient

