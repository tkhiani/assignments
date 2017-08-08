library("datasets")
library("fBasics")

# Identify the data types in iris and explore the data
irisCopy <- read.csv("./learning/Iris - ClassificationClustering/iris.csv")
str(irisCopy)
head(irisCopy)

# Summary statistics of the data
summary(irisCopy)
skewness(irisCopy)
kurtosis(irisCopy)

"
plot(irisCopy, col=irisCopy$Species)
legend(7,4.3, unique(irisCopy$Species), col = 1:length(irisCopy$Species), pch = 1)

par(mfrow=c(2,2))
for(i in 1:4) boxplot(irisCopy[,i] ~ Species, data=irisCopy, main=names(irisCopy)[i])
"

# data preparation
irisCopy$target <- irisCopy$Species

# split the data into train & test
library(dplyr)

train <- sample_frac(irisCopy, 0.7)
train <- train[,-5]
head(train)
test <- train[-as.numeric(rownames(train)),]
head(train)
head(test)

# Determine the bucket size and the minsplit
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

# prune the tree to avoid overfitting CP - 0.015
classificationModel <- prune(classificationModel, cp = 0.015 ,"CP")
printcp(classificationModel)
fancyRpartPlot(classificationModel)

# predict the target for the development sample
train$pTarget <- predict(classificationModel, train, type="class")
predictionScore <- predict(classificationModel, train)
train$pPositiveScore <- predictionScore[,2]

"
confusion matrix to determine model performance
https://www.analyticsvidhya.com/blog/2016/02/7-important-model-evaluation-error-metrics/
"
library("caret")
confusionMatrix(train$pTarget, train$target)

# predict the target for the test sample
head(test)
test$pTarget <- predict(classificationModel, test, type="class")

predictionScore <- predict(classificationModel, test)
predictionScore
confusionMatrix(test$pTarget, test$target)