library(psych)
library(dplyr)
library(randomForest)
library(neuralnet)
library(caret)
library(Deducer)
library(dummies)
library(ROCR)

# Load employee attrition data
attritionData <- read.csv("./hrEmployeeAttrition/HR_Employee_Attrition_Data.csv")

# Remove fields that are not relevant for classificaiton employee number and employee count
head(attritionData)
dim(attritionData)
str(attritionData)

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

for(i in factorFields) 
  attritionData[,i] <- as.factor(attritionData[,i])

attritionNumericData <- data.frame(scale(attritionData[,numericFields])) 
attritionFactorsData <- attritionData[,factorFields]
attritionData <- data.frame(attritionFactorsData, attritionNumericData)

str(attritionData)

rm(attritionFactorsData)
rm(attritionNumericData)

# Exploratory Data Analysis
summary(attritionData)
describe(attritionData)
describeBy(attritionData, group = attritionData$Attrition)

# Convernt no to 0 and yes to 1 for attrition
attritionData$Attrition <- ifelse(attritionData$Attrition =="Yes", 1, 0)
attritionData$Attrition <- as.factor(attritionData$Attrition)

# Split the data into train & test
train <- sample_frac(attritionData, 0.7) 
test <- train[-as.numeric(rownames(train)),]

rm(attritionData)

# Attrition Rate
trainSize <- nrow(train)
attritionRate <- sum(train$Attrition == "1") / trainSize

trainSize
attritionRate

# RF Model to classify 
nSize <- round(1*trainSize/100,0)
describe(train)

rfModel <- randomForest(train$Attrition~.,
             data = train[,-1],
             ntree = 500,
             mtry = 20, 
             nodesize = nSize,
             importance = TRUE)

rfModel
varImpPlot(rfModel)
plot(rfModel)

# Tune the RF model to determine the number of varialbles that need to be randomly sampled at each split
tRFModel <- tuneRF(x = train[,-1], 
              y = train$Attrition, 
              mtryStart = 3, 
              stepFactor = 1.5, 
              ntreeTry = 500, 
              nodeSize = nSize,
              doBest = TRUE,
              importance = TRUE,
              plot = TRUE,
              improve = 0.0001,
              trace = TRUE)

varImpPlot(tRFModel)
varImp(tRFModel)
tRFModel$importance
tRFModel$mtry

# predict the attrition for the train data
probAttritionTrainRF <- predict(tRFModel, train, type = "prob")
pAttritionTrainRF <- predict(tRFModel, train, type = "response")

# rank to determine which decile and k-s value of the training set
trainWithDecile <- train
trainWithDecile$probAttritionTrain <- probAttritionTrainRF[,2]
trainWithDecile <- mutate(trainWithDecile, deciles = ntile(trainWithDecile$probAttritionTrain, 10))
trainWithDecile$pAttrition <- pAttritionTrainRF 
sampleSize <- nrow(trainWithDecile)  
totalLeft <- sum(trainWithDecile$Attrition == "1")
totalRetained <- sum(trainWithDecile$Attrition == "0")

trainByDecile <- group_by(trainWithDecile, deciles)
trainByDecile <- summarise(trainByDecile, 
                               n = n(), 
                               left = sum(Attrition=="1"), 
                               retained = sum(Attrition=="0"), 
                               pLeft = sum(pAttrition=="1"),
                               pRetained = sum(pAttrition=="0"),   
                               rateLeft = (left/totalLeft)*100,
                               rateRetained = (retained/totalRetained)*100
)
trainByDecile <- arrange(trainByDecile, desc(deciles))
trainByDecile <- mutate(trainByDecile, 
                            cumRateRetained = cumsum(rateRetained),
                            cumRateLeft = cumsum(rateLeft),
                            perPopulation = (n/sampleSize)*100,
                            cumPerPopulation = cumsum(perPopulation),
                            liftAtDecile = (rateLeft/perPopulation)*100,
                            totalLift = (cumRateLeft/cumPerPopulation)*100,
                            ks = abs(cumRateLeft - cumRateRetained)
)
View(trainByDecile)

# evaluate performance using a confusion matrix
confusionMatrix(pAttritionTrainRF, train$Attrition, positive = "1")
# evaluate performance using ROC curve, KS, AUC curve
aucTRF <- roc(pAttritionTrainRF,probAttritionTrainRF[,2])
plot(aucTRF)
# Need to still look at others liks KS, Gini Index...

# Predict for test data
pAttritionTestRF <- predict(tRFModel, test, type = "response")
probAttritionTestRF <- predict(tRFModel, test, type = "prob")

# Evaludate performance for test data using a confusion matrix
confusionMatrix(pAttritionTestRF, test$Attrition, positive = "1")

# Work on a Neural Network Model for which we need to create dummies as it does not take categorical data
trainWithDummies <- dummy.data.frame(train[,-1], sep = ".")
trainWithDummies <- setNames(trainWithDummies, make.names(names(trainWithDummies), unique = TRUE))

trainWithDummies$Attrition <- as.integer(train$Attrition) - 1
n <- names(trainWithDummies)
f <- as.formula(paste("Attrition ~", paste(n[!n %in% "Attrition"], collapse = " + ")))
nModel <- neuralnet(f,
          data = trainWithDummies,
          hidden = c(49,7), 
          threshold = 0.001,
          linear.output = FALSE,
          err.fct = "sse",
          stepmax = 2000,
          rep = 5)

probAttritionTrainNN <- compute(nModel, trainWithDummies[,-1])
pAttritionTrainNN <- round(probAttritionTrainNN$net.result, digits = 0)
confusionMatrix(pAttritionTrainNN, train$Attrition, positive = "1")

# Predict attrition on the test data and evaluate it's performance
testWithDummies <- dummy.data.frame(test[,-1], sep = ".")
testWithDummies <- setNames(testWithDummies, make.names(names(testWithDummies), unique = TRUE))

probAttritionTestNN <- compute(nModel, testWithDummies)
pAttritionTestNN <- round(probAttritionTestNN$net.result, digits = 0)
confusionMatrix(pAttritionTestNN, test$Attrition, positive = "1")

# Ensemble Model - averaging the probabilities as we only have two algorithms. If we had three I would have used a voting principle  
jointProbTestData <- (probAttritionTestRF[,2] + probAttritionTestNN$net.result) / 2
jointPAttritionTest <- round(jointProbTestData, digits = 0)

# Evaluating the performance of the model via a confusion matrix
confusionMatrix(jointPAttritionTest, test$Attrition, positive = "1")
