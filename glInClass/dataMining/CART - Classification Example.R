library("rpart")
library("rpart.plot")
library("dplyr")
library("caret")
library("RColorBrewer")
library("ROCR")

"
View the tree
issues with library(rattle)
"


# Import the development sample
devSample <- read.csv("./glInClass/dataMining/DEV_SAMPLE.csv", sep = ",", header = TRUE)
head(devSample)

## Response Rate 
responseRate <- sum(devSample$Target)/nrow(devSample)
responseRate

# specifying the criteria
controlCriteria = rpart.control(minsplit=100, minbucket = 10, cp = 0, xval = 10)

# build the decison tree based on CART
classificationModel <- rpart(formula = Target ~ ., data = devSample[,-1], method = "class", control = controlCriteria)
classificationModel
fancyRpartPlot(classificationModel)

# use the CP value where the xerror is the least
printcp(classificationModel)
plotcp(classificationModel)

# prune the tree to avoid overfitting CP - 0.00202429
classificationModel <- prune(classificationModel, cp = 0.0021 ,"CP")
printcp(classificationModel)
fancyRpartPlot(classificationModel)

# predict the target for the development sample
devSample$pTarget <- predict(classificationModel, devSample, type="class")
predictionScore <- predict(classificationModel, devSample)
devSample$pPositiveScore <- predictionScore[,2]

" 
Scoring the model based on the development sample
lift chart to determine model performance
Need help on how to draw charts: https://www.analyticsvidhya.com/blog/2016/02/7-important-model-evaluation-error-metrics/
"
devSample <- mutate(devSample, deciles = ntile(pPositiveScore, 10))
View(devSample)
sampleSize <- nrow(devSample)  
responseRate <- (sum(devSample$Target)/sampleSize)*100
noOfResponseForResponders <- sum(devSample$Target==1)
noOfResponseForNonResponders <- sum(devSample$Target==0)
devSampleByDecile <- group_by(devSample[,-1], deciles)
devSampleByDecile <- summarise(devSampleByDecile, 
                               n = n(), 
                               resonders = sum(Target==1), 
                               nonResponders = sum(Target==0), 
                               pResponders = sum(pTarget==1),
                               pNonResponders = sum(pTarget==0),   
                               responseRateForNonResponders = (nonResponders/noOfResponseForNonResponders)*100,
                               responseRateForResponders = (resonders/noOfResponseForResponders)*100
                               )
devSampleByDecile <- arrange(devSampleByDecile, desc(deciles))
devSampleByDecile <- mutate(devSampleByDecile, 
                            cumResponseRateForResponders = cumsum(responseRateForResponders),
                            cumResponseRateForNonResponders = cumsum(responseRateForNonResponders),
                            perPopulation = (n/sampleSize)*100,
                            cumPerPopulation = cumsum(perPopulation),
                            liftAtDecile = (responseRateForResponders/perPopulation)*100,
                            totalLift = (cumResponseRateForResponders/cumPerPopulation)*100,
                            ks = (cumResponseRateForResponders - cumResponseRateForNonResponders)
                            )
View(devSampleByDecile)

"
confusion matrix to determine model performance
https://www.analyticsvidhya.com/blog/2016/02/7-important-model-evaluation-error-metrics/
"
confusionMatrix(devSample$pTarget, devSample$Target)

"
Kolomogorov Smirnov chart
"
plot(devSampleByDecile$deciles, devSampleByDecile$ks, type = "l")


"
Other methods to determine model performance
ROC curve on the other hand is almost independent of the response rate
AUC - Area under curve
Gini Coffecient -
"
predition <- prediction(devSample$pPositiveScore, devSample$Target)
tprFpr <- performance(predition, "tpr", "fpr")
plot(tprFpr)
KS <- max(attr(tprFpr, 'y.values')[[1]]-attr(tprFpr, 'x.values')[[1]])
auc <- performance(predition,"auc"); 
auc <- as.numeric(auc@y.values)
giniCoffecient <- (2*auc)-1


# Import the holdout sample
holdOutSample <- read.csv("./glInClass/dataMining/HOLDOUT_SAMPLE.csv", sep = ",", header = TRUE)
head(holdOutSample)

# predict the target for the holdout sample
holdOutSample$pTarget <- predict(classificationModel, holdOutSample, type="class")
predictionScore <- predict(classificationModel, holdOutSample)
holdOutSample$pPositiveScore <- predictionScore[,2]

" 
Scoring the model based on the development sample
lift chart to determine model performance
Need help on how to draw charts: https://www.analyticsvidhya.com/blog/2016/02/7-important-model-evaluation-error-metrics/
"
holdOutSample <- mutate(holdOutSample, deciles = ntile(pPositiveScore, 10))
View(holdOutSample)
sampleSize <- nrow(holdOutSample)  
responseRate <- (sum(holdOutSample$Target)/sampleSize)*100
noOfResponseForResponders <- sum(holdOutSample$Target==1)
noOfResponseForNonResponders <- sum(holdOutSample$Target==0)
holdOutSampleByDecile <- group_by(holdOutSample[,-1], deciles)
holdOutSampleByDecile <- summarise(holdOutSampleByDecile, 
                               n = n(), 
                               resonders = sum(Target==1), 
                               nonResponders = sum(Target==0), 
                               pResponders = sum(pTarget==1),
                               pNonResponders = sum(pTarget==0),   
                               responseRateForNonResponders = (nonResponders/noOfResponseForNonResponders)*100,
                               responseRateForResponders = (resonders/noOfResponseForResponders)*100
)
holdOutSampleByDecile <- arrange(holdOutSampleByDecile, desc(deciles))
holdOutSampleByDecile <- mutate(holdOutSampleByDecile, 
                            cumResponseRateForResponders = cumsum(responseRateForResponders),
                            cumResponseRateForNonResponders = cumsum(responseRateForNonResponders),
                            perPopulation = (n/sampleSize)*100,
                            cumPerPopulation = cumsum(perPopulation),
                            liftAtDecile = (responseRateForResponders/perPopulation)*100,
                            totalLift = (cumResponseRateForResponders/cumPerPopulation)*100,
                            ks = (cumResponseRateForResponders - cumResponseRateForNonResponders)
)
View(holdOutSampleByDecile)

"
Other methods to determine model performance
ROC curve on the other hand is almost independent of the response rate
AUC - Area under curve
Gini Coffecient -
"
hPredition <- prediction(holdOutSample$pPositiveScore, holdOutSample$Target)
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
confusionMatrix(devSample$pTarget, devSample$Target)
confusionMatrix(holdOutSample$pTarget, holdOutSample$Target)

"
Comparing dev & hold out
"

KS
hKS

auc
hAuc

giniCoffecient
hGiniCoffecient
