library(psych)
library(dplyr)
library(randomForest)
library(neuralnet)
library(caret)
library(Deducer)
library(dummies)
library(ROCR)
library(pROC)
library(corrplot)
library(ggplot2)
library(gridExtra)

# Load employee attrition data
attritionData <- read.csv("./hrEmployeeAttrition/HR_Employee_Attrition_Data.csv")

# Exploratory Data Analysis

# Remove fields that are not relevant for classificaiton employee number and employee count
head(attritionData)
dim(attritionData)
str(attritionData)
summary(attritionData)
describe(attritionData)
describeBy(attritionData, group = attritionData$Attrition)

# Convert factor fields to factors and scale numeric fields
# Remove employee count, standard hours, over 18 as it is not relevant for analysis
factorFields <- c("Attrition", "BusinessTravel", "Department", "Education", "EducationField", "EnvironmentSatisfaction", "Gender", 
                  "JobInvolvement", "JobLevel", "JobRole", "JobSatisfaction", "MaritalStatus", "OverTime", 
                  "PerformanceRating","RelationshipSatisfaction", "StockOptionLevel", "WorkLifeBalance")

numericFields <- c("Age", "DailyRate", "DistanceFromHome", "HourlyRate", "MonthlyIncome", "MonthlyRate", "NumCompaniesWorked", 
                   "PercentSalaryHike", "TotalWorkingYears", "TrainingTimesLastYear", "YearsAtCompany", 
                   "YearsInCurrentRole", "YearsSinceLastPromotion", "YearsWithCurrManager")

employeeNumber <- attritionData$EmployeeNumber
attritionData$EmployeeCount <- NULL
attritionData$StandardHours <- NULL
attritionData$Over18 <- NULL

for(i in factorFields) {
  attritionData[,i] <- as.factor(attritionData[,i])
  if(i != "Attrition")
    print(ggplot() + 
                 geom_bar(aes(y = ..count..,x =attritionData[,i], fill = Attrition), 
                          data = attritionData, position = position_stack()) + xlab(i))
}

attritionNumericData <- data.frame(scale(attritionData[,numericFields]))
attritionFactorsData <- attritionData[,factorFields]
attritionData <- data.frame(attritionFactorsData, attritionNumericData)

str(attritionData)

rm(attritionFactorsData)
rm(attritionNumericData)


r <- round(cor(attritionData[,numericFields]),2)
# The following are highly correlated
# Age - Total Working Years
# Total Working Years - Monthly Income, Age, Years at Company
# Years In Current Role - Years at Company, Years with Current Manager
# Years with current manager - Years in current role, Years at company
corrplot(r, order = "hclust", tl.col='black', tl.cex=.75) 

# Convernt no to 0 and yes to 1 for attrition
attritionData$Attrition <- ifelse(attritionData$Attrition =="Yes", 1, 0)
attritionData$Attrition <- as.factor(attritionData$Attrition)
attritionRate <- sum(attritionData$Attrition == "1") / nrow(attritionData)
attritionRate

# Split the data into dev & hOut
set.seed(9090)
dev <- sample_frac(attritionData, 0.7) 
hOut <- attritionData[-as.numeric(rownames(dev)),]

rm(attritionData)

# Attrition Rate
devSize <- nrow(dev)
attritionRate <- sum(dev$Attrition == "1") / devSize

devSize
attritionRate

# RF Model to classify 
nSize <- round(3*devSize/100,0)
nSize
rfModel <- randomForest(dev$Attrition~.,
             data = dev[,-1],
             ntree = 500,
             mtry = 10, 
             nodesize = nSize,
             importance = TRUE)

rfModel
plot(rfModel)

# Tune the RF model to determine the number of varialbles that need to be randomly sampled at each split
tRFModel <- tuneRF(x = dev[,-1], 
              y = dev$Attrition, 
              mtryStart = 3, 
              stepFactor = 1.5, 
              ntreeTry = 300, 
              nodeSize = nSize,
              doBest = TRUE,
              importance = TRUE,
              plot = TRUE,
              improve = 0.001,
              trace = TRUE)

varImpPlot(tRFModel)
varImp(tRFModel)
tRFModel$mtry

# predict the attrition for the dev data
probAttritiondevRF <- tRFModel$votes[,2]
pAttritiondevRF <- tRFModel$predicted

# rank to determine which decile and k-s value of the deving set
devWithDecile <- dev
devWithDecile$probAttritiondev <- probAttritiondevRF
devWithDecile <- mutate(devWithDecile, deciles = ntile(devWithDecile$probAttritiondev, 10))
devWithDecile$pAttrition <- pAttritiondevRF 
sampleSize <- nrow(devWithDecile)  
totalLeft <- sum(devWithDecile$Attrition == "1")
totalRetained <- sum(devWithDecile$Attrition == "0")

devByDecile <- group_by(devWithDecile, deciles)
devByDecile <- summarise(devByDecile, 
                               n = n(), 
                               left = sum(Attrition=="1"), 
                               retained = sum(Attrition=="0"), 
                               pLeft = sum(pAttrition=="1"),
                               pRetained = sum(pAttrition=="0"),   
                               rateLeft = (left/totalLeft)*100,
                               rateRetained = (retained/totalRetained)*100
)
devByDecile <- arrange(devByDecile, desc(deciles))
devByDecile <- mutate(devByDecile, 
                            cumRateRetained = cumsum(rateRetained),
                            cumRateLeft = cumsum(rateLeft),
                            perPopulation = (n/sampleSize)*100,
                            cumPerPopulation = cumsum(perPopulation),
                            liftAtDecile = (rateLeft/perPopulation)*100,
                            totalLift = (cumRateLeft/cumPerPopulation)*100,
                            ks = abs(cumRateLeft - cumRateRetained)
)
View(devByDecile)

# evaluate performance using a confusion matrix
confusionMatrix(pAttritiondevRF, dev$Attrition, positive = "1")
# evaluate performance using ROC curve, KS, AUC curve

# Plot the performance of the model applied to the evaluation set as
# an ROC curve.
pred <- prediction(probAttritiondevRF, dev$Attrition)
plot(performance(pred,"lift","rpp"), main="lift curve", colorize=T)
aucRFdev <- roc(dev$Attrition,probAttritiondevRF)
aucRFdev
ksRFdev <- max(devByDecile$ks)
ksRFdev
plot(aucRFdev)

# Predict for hOut data
pAttritionhOutRF <- predict(tRFModel, hOut, type = "response")
probAttritionhOutRF <- predict(tRFModel, hOut, type = "prob")

# Evaludate performance for hOut data using a confusion matrix
confusionMatrix(pAttritionhOutRF, hOut$Attrition, positive = "1")
aucRFhOut <- roc(hOut$Attrition,probAttritionhOutRF[,2])
aucRFhOut
plot(aucRFhOut)
pred <- prediction(probAttritionhOutRF[,2], hOut$Attrition)
plot(performance(pred,"lift","rpp"), main="lift curve", colorize=T)

# Work on a Neural Network Model for which we need to create dummies as it does not take categorical data
devWithDummies <- dummy.data.frame(dev[,-1], sep = ".")
devWithDummies <- setNames(devWithDummies, make.names(names(devWithDummies), unique = TRUE))

devWithDummies$Attrition <- as.integer(dev$Attrition) - 1
n <- names(devWithDummies)
f <- as.formula(paste("Attrition ~", paste(n[!n %in% "Attrition"], collapse = " + ")))
f
nCols <- ncol(devWithDummies)
nCols
nModel <- neuralnet(f,
          data = devWithDummies,
          hidden = 52, 
          threshold = 0.00001,
          linear.output = FALSE,
          err.fct = "sse",
          stepmax = 3000,
          lifesign = "full",
          lifesign.step = 10)

probAttritiondevNN <- compute(nModel, devWithDummies[,-1])
pAttritiondevNN <- round(probAttritiondevNN$net.result, digits = 0)
# performance of the NN model with dev data
confusionMatrix(pAttritiondevNN[,1], dev$Attrition, positive = "1")
aucNNdev <- roc(dev$Attrition,probAttritiondevNN$net.result)
aucNNdev
plot(aucNNdev)
pred <- prediction(probAttritiondevNN$net.result, dev$Attrition)
plot(performance(pred,"lift","rpp"), main="lift curve", colorize=T)

# Predict attrition on the hOut data and evaluate it's performance
hOutWithDummies <- dummy.data.frame(hOut[,-1], sep = ".")
hOutWithDummies <- setNames(hOutWithDummies, make.names(names(hOutWithDummies), unique = TRUE))

probAttritionhOutNN <- compute(nModel, hOutWithDummies)
pAttritionhOutNN <- round(probAttritionhOutNN$net.result, digits = 0)
# Evaludate performance for hOut data 
confusionMatrix(pAttritionhOutNN, hOut$Attrition, positive = "1")
aucNNhOut <- roc(hOut$Attrition,probAttritionhOutNN$net.result)
aucNNhOut
plot(aucNNhOut)
pred <- prediction(probAttritionhOutNN$net.result, hOut$Attrition)
plot(performance(pred,"lift","rpp"), main="lift curve", colorize=T)

# Ensemble Model - averaging the probabilities as we only have two algorithms.
# If we had three I would have used a voting principle  
jointProbdev <- (probAttritiondevRF + probAttritiondevNN$net.result) / 2
jointPAttritiondev <- round(jointProbdev, digits = 0)

# Evaluating the performance of the model for the dev data via a confusion matrix
confusionMatrix(jointPAttritiondev, dev$Attrition, positive = "1")
auchOut <- roc(dev$Attrition,jointProbdev)
auchOut
plot(auchOut)
pred <- prediction(jointProbdev, dev$Attrition)
plot(performance(pred,"lift","rpp"), main="lift curve", colorize=T)


# Ensemble Model - averaging the probabilities as we only have two algorithms.
# If we had three I would have used a voting principle  
jointProbhOut <- (probAttritionhOutRF[,2] + probAttritionhOutNN$net.result) / 2
jointPAttritionhOut <- round(jointProbhOut, digits = 0)

# Evaluating the performance of the model via a confusion matrix
confusionMatrix(jointPAttritionhOut, hOut$Attrition, positive = "1")
auchOut <- roc(hOut$Attrition,jointProbhOut)
auchOut
plot(auchOut)
pred <- prediction(jointProbhOut, hOut$Attrition)
plot(performance(pred,"lift","rpp"), main="lift curve", colorize=T)

