library(psych)
library(rpart)
library(rpart.plot)
library(rattle)
library(caret)
library(pROC)
library(randomForest)
library(dplyr)
library(dummies)
library(ROCR)
library(neuralnet)
library(ggplot2)

# load the dev and hold out
devSample <- read.csv("./holdOutAndDevClassificationExample/DEV_SAMPLE.csv")
holdOut <- read.csv("./holdOutAndDevClassificationExample/HOLDOUT_SAMPLE.csv")

# exploratory data analysis
summary(devSample)
describe(devSample)

# factor - Target, Gender, Occupation, AGE_BKT
# numeric - Balance, No_OF_CR_TXNS, SCR, Holding_Period
# Avoid the following columns - Cust_ID, AGE_BKT (as age is available) 
fFields <- c("Target", "Gender", "Occupation") 
nFields <- c("Balance", "No_OF_CR_TXNS", "SCR", "Holding_Period", "Age")

dCustID <- devSample$Cust_ID
hCustID <- holdOut$Cust_ID

for(i in fFields) {
  devSample[,i] <- as.factor(devSample[,i])
  holdOut[,i] <- as.factor(holdOut[,i])   
}

for(i in fFields) {
  if(i != "Target")
    print(ggplot() + 
            geom_bar(aes(y = ..count..,x =devSample[,i], fill = Target), 
                     data = devSample, position = position_stack()) + xlab(i))
}


for(i in nFields) {
  print(ggplot(data = devSample, aes(devSample[,i], fill = Target)) + geom_histogram(bins = 20) + xlab(i))
}


# Scale the data as we have large variations
dev <- data.frame(devSample[,fFields], scale(devSample[,nFields]))
hOut <- data.frame(holdOut[,fFields], scale(holdOut[,nFields]))

library(caTools)
set.seed(101)

successRate <- sum(dev$Target=="1")/nrow(dev) 
successRate

# Randomly select 50% of non-positive responses. The idea is to have a sucess rate doubled
zeros <- filter(dev, Target == "0")
ones <- filter(dev, Target == "1")
split = sample.split(zeros$Target, SplitRatio = 0.50)

# Split based off of split Boolean Vector
zeros = subset(zeros, split == TRUE)
dev <- rbind(ones, zeros)

rm(devSample)
rm(holdOut)

# determine the bucket size and the minsplit

successRate <- sum(dev$Target=="1")/nrow(dev) 
successRate

sizeOfTheDataSet <- nrow(dev)
sizeOfTheDataSet

minBucketSize <- round(0.5*sizeOfTheDataSet/100, 0) 
minBucketSize

minSplitSize <- 3*minBucketSize  
minSplitSize

# specifying the criteria - minBucket: 2-3% of pupulation, minSplit: 3*minBucket
controlCriteria = rpart.control(
  minsplit=minSplitSize, 
  minbucket = minBucketSize, 
  cp = 0, 
  xval = 10)

# build the decison tree based on CART
cartModel <- rpart(
  formula = Target ~ ., 
  data = dev, 
  method = "class", 
  control = controlCriteria)

cartModel

fancyRpartPlot(cartModel)

# use the CP value where the xerror is the least
printcp(cartModel)
plotcp(cartModel)

# prune the tree
classificationModel <- prune(cartModel, cp = 0.0081,"CP")
printcp(classificationModel)
fancyRpartPlot(classificationModel)

# predict the target for the deving sample
pCartTarget <- predict(classificationModel, dev, type="class")
probCartTarget <- predict(classificationModel, dev, type = "prob")

confusionMatrix(pCartTarget, dev$Target, positive = "1")

auc(dev$Target, probCartTarget[,2])
plot.roc(roc(dev$Target, probCartTarget[,2]))

pred <- ROCR::prediction(probCartTarget[,2], dev[,"Target"])
plot(performance(pred,"lift","rpp"), main="lift curve", colorize=T)

# predict the target for the hOut sample
pCartTarget <- predict(classificationModel, hOut, type="class")
probCartTarget <- predict(classificationModel, hOut, type = "prob")

confusionMatrix(pCartTarget, hOut$Target, positive = "1")

auc(hOut$Target, probCartTarget[,2])
plot.roc(roc(hOut$Target, probCartTarget[,2]))
pred <- ROCR::prediction(probCartTarget[,2], hOut$Target)
plot(performance(pred,"lift","rpp"), main="lift curve", colorize=T)


# Random Forests
nSize <- 0.5*sizeOfTheDataSet/100 
nSize
rfModel <- randomForest(dev$Target~.,
                        data = dev[,-1],
                        ntree = 500,
                        mtry = 4, 
                        nodesize = nSize,
                        importance = TRUE)

rfModel
varImpPlot(rfModel)
plot(rfModel)

# Tune the RF model to determine the number of varialbles that need to be randomly sampled at each split
tRFModel <- tuneRF(x = dev[,-1], 
                   y = dev$Target, 
                   mtryStart = 3, 
                   stepFactor = 1.5, 
                   ntreeTry = 200, 
                   nodeSize = nSize,
                   doBest = TRUE,
                   importance = TRUE,
                   plot = TRUE,
                   improve = 0.001,
                   trace = TRUE)

varImpPlot(tRFModel)
varImp(tRFModel)
tRFModel$importance

# predict the target for the dev sample
pdRFTarget <- tRFModel$predicted
probdRFTarget <- tRFModel$votes

confusionMatrix(pdRFTarget, dev$Target, positive = "1")

auc(dev$Target, probdRFTarget[,2])
plot(roc(dev$Target,probdRFTarget[,2]))
pred <- ROCR::prediction(probdRFTarget[,2], dev$Target)
plot(performance(pred,"lift","rpp"), main="lift curve", colorize=T)

# predict the target for the hOut sample
phRFTarget <- predict(tRFModel, hOut, type="class")
probhRFTarget <- predict(tRFModel, hOut, type = "prob")

confusionMatrix(phRFTarget, hOut$Target, positive = "1")

auc(hOut$Target, probhRFTarget[,2])
plot(roc(hOut$Target,probhRFTarget[,2]))
pred <- ROCR::prediction(probhRFTarget[,2], hOut$Target)
plot(performance(pred,"lift","rpp"), main="lift curve", colorize=T)

# Neural Network
head(dev)
devWithDummies <- dummy.data.frame(dev[,-1], sep = ".")
devWithDummies <- setNames(devWithDummies, make.names(names(devWithDummies), unique = TRUE))

devWithDummies$Target <- as.integer(dev$Target) - 1
n <- names(devWithDummies)
f <- as.formula(paste("Target ~", paste(n[!n %in% "Target"], collapse = " + ")))
f
nModel <- neuralnet(f,
                    data = devWithDummies,
                    hidden = 9, 
                    threshold = 0.01,
                    linear.output = FALSE,
                    err.fct = "sse",
                    stepmax = 20000,
                    lifesign = "full",
                    lifesign.step = 1000)

probAttritiondevNN <- compute(nModel, devWithDummies[,-1])
pAttritiondevNN <- round(probAttritiondevNN$net.result, digits = 0)
# performance of the NN model with dev data
confusionMatrix(pAttritiondevNN, dev$Target, positive = "1")
aucNNhOut <- roc(dev$Target,probAttritiondevNN$net.result)
aucNNhOut
plot(aucNNhOut)
pred <- ROCR::prediction(probAttritiondevNN$net.result, dev$Target)
plot(performance(pred,"lift","rpp"), main="lift curve", colorize=T)

# Predict attrition on the hOut data and evaluate it's performance
hOutWithDummies <- dummy.data.frame(hOut[,-1], sep = ".")
hOutWithDummies <- setNames(hOutWithDummies, make.names(names(hOutWithDummies), unique = TRUE))

probAttritionhOutNN <- compute(nModel, hOutWithDummies)
pAttritionhOutNN <- round(probAttritionhOutNN$net.result, digits = 0)

# Evaludate performance for hOut data 
confusionMatrix(pAttritionhOutNN, hOut$Target, positive = "1")
aucNNhOut <- roc(hOut$Target,probAttritionhOutNN$net.result)
aucNNhOut
plot(aucNNhOut)
pred <- ROCR::prediction(probAttritionhOutNN$net.result, hOut$Target)
plot(performance(pred,"lift","rpp"), main="lift curve", colorize=T)

# Evaludate performance of enseble model on the hold out sample
ensembleProbability <- data.frame(probCartTarget[,2], probhRFTarget[,2], 
                                      probAttritionhOutNN$net.result)
ensembleProbability <- apply(ensembleProbability, 1, max)
ensembleP <- ifelse(ensembleProbability < 0.5, "0", "1")
confusionMatrix(ensembleP, hOut$Target, positive = "1")
aucEnsemble <- roc(hOut$Target,ensembleProbability)
aucEnsemble
plot(aucEnsemble)
pred <- ROCR::prediction(ensembleProbability, hOut$Target)
plot(performance(pred,"lift","rpp"), main="lift curve", colorize=T)


