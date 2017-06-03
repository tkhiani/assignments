# Understanding how clustering can improve the performance of classification 
# https://www.analyticsvidhya.com/blog/2016/11/an-introduction-to-clustering-and-different-methods-of-clustering/

library(randomForest)
library(caret)
library(Hmisc)
library(Metrics)

data <- read.csv("../largeDataFiles/stocks/stock_data.csv", stringsAsFactors = TRUE)
dim(data)
data$Y <- as.factor(data$Y)

dataForClustering <- data[,-101]
dataForClustering <- data.matrix(dataForClustering)
clusterExampes <- varclus(dataForClustering)
plot(clusterExampes)

train <- data[1:2000,]
test <- data[2001:3000,]

# Classify using random forest
model_rf <- randomForest(Y~.,data = train)

preds <- predict(model_rf, test[,-101])
r1 <- confusionMatrix(preds, test$Y)

cluster <- kmeans(data[,-101], 5)
data$cluster <- as.factor(cluster$cluster)

#dividing the dataset into train and test
train <- data[1:2000,]
test <- data[2001:3000,]

#applying randomforest
model_rf <- randomForest(Y~., data=train)

preds2 <- predict(model_rf, test[,-101])
r2 <- confusionMatrix(preds2, test$Y)

auc(test$Y, preds)
auc(test$Y, preds2)
