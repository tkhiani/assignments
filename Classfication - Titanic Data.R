# load data
aTrain <- read.csv("./data/titanic/train.csv")
str(aTrain)
aTrain$Pclass <- as.factor(aTrain$Pclass)
str(aTrain)
head(aTrain)
summary(aTrain)
aTrain$Age[is.na(aTrain$Age)] <- median(aTrain$Age, na.rm=TRUE)

# Ignoring columns that are not needed for classification and slipt into two
library(dplyr)
train <- sample_frac(aTrain, 0.7)
train <- train[,-c(1,4,9,11)]
test <- aTrain[-as.numeric(rownames(train)),]
test <- test[,-c(1,4,9,11)]
head(train)
head(test)

# Summary statistics of the data
library(psych)
summary(train)
describe(train)

# Scale data for classification
temp <- train[,c(1,2,3,8)]
sTrain <- data.frame(scale(train[,c(4,5,6,7)]))
train <- bind_cols(temp, sTrain)
head(train)

# response rate of the training data set & determine the bucket size and the minsplit
survivalRate <- sum(train$Survived)/nrow(train) 
survivalRate

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
classificationModel <- rpart(formula = Survived ~ ., data = train, method = "class", control = controlCriteria)
classificationModel
library("rpart.plot")
library("rattle")
fancyRpartPlot(classificationModel)

# use the CP value where the xerror is the least
printcp(classificationModel)
plotcp(classificationModel)

# prune the tree
classificationModel <- prune(classificationModel, cp = 0.017 ,"CP")
printcp(classificationModel)
fancyRpartPlot(classificationModel)

# predict the target for the training sample
train$pSurvived <- predict(classificationModel, train, type="class")
predictionScore <- predict(classificationModel, train)
train$pPositiveScore <- predictionScore[,2]

# confusion matrix to determine model performance: https://www.analyticsvidhya.com/blog/2016/02/7-important-model-evaluation-error-metrics/
library("caret")
confusionMatrix(train$pSurvived, train$Survived)


# Scale hold-out data for classification
temp <- test[,c(1,2,3,8)]
sTest <- data.frame(scale(test[,c(4,5,6,7)]))
test <- bind_cols(temp, sTest)
head(test)

# predict the target for the hold-out sample
test$pSurvived <- predict(classificationModel, test, type="class")
predictionScore <- predict(classificationModel, test)
test$pPositiveScore <- predictionScore[,2]

# confusion matrix to determine model performance: https://www.analyticsvidhya.com/blog/2016/02/7-important-model-evaluation-error-metrics/
confusionMatrix(test$pSurvived, test$Survived)

# Testing it out in the production sample
aProduction <- read.csv("./data/titanic/test.csv")
aProduction$Pclass <- as.factor(aProduction$Pclass)
head(aProduction)
summary(aProduction)
aProduction$Age[is.na(aProduction$Age)] <- median(aProduction$Age, na.rm=TRUE)

# Scale data for classification
pData <- aProduction[,-c(1,3,8,10)]
head(pData)
temp <- pData[,c(1,2,7)]
sTemp <- data.frame(scale(pData[,c(3,4,5,6)]))
pData <- bind_cols(temp, sTemp)
head(pData)

# predict the target for the training sample
pData$pSurvived <- predict(classificationModel, pData, type="class")
predictionScore <- predict(classificationModel, pData)
pData$pPositiveScore <- predictionScore[,2]

# club the classified result with the production data
aProduction$Survived <- pData$pSurvived
View(aProduction)
write.csv(aProduction, file = "./assignment details/Titanic-Results.csv")
