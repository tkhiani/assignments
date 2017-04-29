# load data
aTrain <- read.csv("./data/loadPrediction/train.csv")
str(aTrain)
aTrain$Credit_History <- as.factor(aTrain$Credit_History)
aTrain$Dependents <- as.numeric(aTrain$Dependents)
summary(aTrain)

# Dropping those rows where Loan Amount and Load Term is not known
library(tidyr)
aTrain <- drop_na(aTrain, LoanAmount, Loan_Amount_Term)
# Let us drop rows where we are not aware of the loan amount and tenure
# aTrain$LoanAmount[is.na(aTrain$LoanAmount)] <- median(aTrain$LoanAmount, na.rm=TRUE)
# aTrain$Loan_Amount_Term[is.na(aTrain$Loan_Amount_Term)] <- median(aTrain$Loan_Amount_Term, na.rm=TRUE)
summary(aTrain)

# Ignoring columns that are not needed for classification and slipt into two
library(dplyr)
train <- sample_frac(aTrain, 0.85)
train <- train[,-c(1)]
holdOut <- aTrain[-as.numeric(rownames(train)),]
holdOut <- holdOut[,-c(1)]
head(train)
head(holdOut)

# Summary statistics of the data
library(psych)
describe(train)


# Decide how to treat outliers

# Work towards normaizing the data
# train$ApplicantIncome <- log10(train$ApplicantIncome+1)
# train$CoapplicantIncome <- log10(train$CoapplicantIncome+1)
# train$LoanAmount <- log10(train$LoanAmount+1)
# train$Loan_Amount_Term <- log10(train$Loan_Amount_Term+1) 
# describe(train)

# Scale data as we do not want one attribute to dominate
temp <- data.frame(train[,c(1,2,4,5,10,11,12)])
stemp <- data.frame(scale(train[,c(3,6,7,8,9)]))
train <- bind_cols(temp, stemp)
describe(train)
head(train)

# determine the bucket size and the minsplit
sizeOfTheDataSet <- nrow(train)
sizeOfTheDataSet

minBucketSize <- 2*sizeOfTheDataSet/100 
minBucketSize

minSplitSize <- 2*minBucketSize  
minSplitSize

# specifying the criteria - minBucket: 2-3% of pupulation, minSplit: 3*minBucket
library("rpart")
controlCriteria = rpart.control(minsplit=minSplitSize, minbucket = minBucketSize, cp = 0, xval = 10)

# build the decison tree based on CART
classificationModel <- rpart(formula = Loan_Status ~ ., data = train, method = "class", control = controlCriteria)
classificationModel
library("rpart.plot")
library("rattle")
fancyRpartPlot(classificationModel)

# use the CP value where the xerror is the least
printcp(classificationModel)
plotcp(classificationModel)

# prune the tree
classificationModel <- prune(classificationModel, cp = 0.0098 ,"CP")
printcp(classificationModel)
fancyRpartPlot(classificationModel)

# predict the target for the training sample
train$pLoan_Status <- predict(classificationModel, train, type="class")
predictionScore <- predict(classificationModel, train)
train$pPositiveScore <- predictionScore[,2]

# confusion matrix to determine model performance: https://www.analyticsvidhya.com/blog/2016/02/7-important-model-evaluation-error-metrics/
library("caret")
confusionMatrix(train$pLoan_Status, train$Loan_Status)

# Work towards normaizing the data
# holdOut$ApplicantIncome <- log10(holdOut$ApplicantIncome+1)
# holdOut$CoapplicantIncome <- log10(holdOut$CoapplicantIncome+1)
# holdOut$LoanAmount <- log10(holdOut$LoanAmount+1)
# holdOut$Loan_Amount_Term <- log10(holdOut$Loan_Amount_Term+1) 
# describe(holdOut)

# Scale data as we do not want one attribute to dominate
temp <- data.frame(holdOut[,c(1,2,4,5,10,11,12)])
stemp <- data.frame(scale(holdOut[,c(3,6,7,8,9)]))
holdOut <- bind_cols(temp, stemp)
describe(holdOut)
head(holdOut)

# predict the target for the hold-out sample
holdOut$pLoan_Status <- predict(classificationModel, holdOut, type="class")
predictionScore <- predict(classificationModel, holdOut)
holdOut$pPositiveScore <- predictionScore[,2]

# confusion matrix to determine model performance: https://www.analyticsvidhya.com/blog/2016/02/7-important-model-evaluation-error-metrics/
confusionMatrix(holdOut$pLoan_Status, holdOut$Loan_Status)

# Testing it out in the production sample
aTest <- read.csv("./data/loadPrediction/test.csv")
loansWithUnknownAmount <- filter(aTest, is.na(Loan_Amount_Term) | is.na(LoanAmount))
aTest <- filter(aTest, !(is.na(Loan_Amount_Term) | is.na(LoanAmount)))

# aTest$LoanAmount[is.na(aTest$LoanAmount)] <- median(aTest$LoanAmount, na.rm=TRUE)
# aTest$Loan_Amount_Term[is.na(aTest$Loan_Amount_Term)] <- median(aTest$Loan_Amount_Term, na.rm=TRUE)
aTest$Credit_History <- as.factor(aTest$Credit_History)
aTest$Dependents <- as.numeric(aTest$Dependents)
test <- aTest[,-c(1)]

# Work towards normaizing the data
# test$ApplicantIncome <- log10(test$ApplicantIncome+1)
# test$CoapplicantIncome <- log10(test$CoapplicantIncome+1)
# test$LoanAmount <- log10(test$LoanAmount+1)
# test$Loan_Amount_Term <- log10(test$Loan_Amount_Term+1) 

# Scale data as we do not want one attribute to dominate
head(test)
temp <- data.frame(test[,c(1,2,4,5,10,11)])
stemp <- data.frame(scale(test[,c(3,6,7,8,9)]))
test <- bind_cols(temp, stemp)

# predict the target for the training sample
test$Loan_Status <- predict(classificationModel, test, type="class")
predictionScore <- predict(classificationModel, test)
test$pPositiveScore <- predictionScore[,2]

# create the output file
loansWithUnknownAmount$Loan_Status <- "N"
loansWithUnknownAmount <- loansWithUnknownAmount[,c(1,13)]
result <- data.frame(Loan_ID = aTest$Loan_ID, Loan_Status = test$Loan_Status)
result <- bind_rows(result,loansWithUnknownAmount)
View(result)
write.csv(result, file = "./assignment details/Loan Prediction.csv")


