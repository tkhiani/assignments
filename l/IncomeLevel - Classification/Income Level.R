library("dplyr")
library("ggplot2")
library("psych")
library("plotly")
library("caret")
library("xgboost")

train <- read.csv("./largeDataSets/incomeLevelClassification/train.csv", na.strings = c(NA, ""," ", "?", "NA"))
head(train)

# Check the levels in the target & its propotions
unique(train$income_level)
train$target <- ifelse(train$income_level==-50000,0,1)
train <- train[,-41] 
size <- nrow(train)
propotionOf1s <- sum(train$target==1)/size
propotionsOf0s <- sum(train$target==0)/size
size
propotionOf1s
propotionsOf0s

# Appropriately change the type of the columns to either factors or numeric as per 
# http://archive.ics.uci.edu/ml/machine-learning-databases/census-income-mld/census-income.names  
factCols <- c(2:5, 7:16, 20:29, 31:38, 40:41)
train[factCols] <- lapply(train[factCols], factor)
trainFactors <- train[,factCols]
namesOfFactors <- names(trainFactors[-34])

trainNumeric <- train[,-c(factCols)]
namesOfNumeric <- names(trainNumeric)
trainNumeric <- bind_cols(trainNumeric, target)
nonFactCols <- setdiff(1:40, factCols)
# Not sure why this does not work - train[nonFactCols] <- lapply(train[nonFactCols], double)

# Understand the data
"
  Age min is 0
  
  dividend_from_Stocks - Max value is very suspicious

  more research needed on capital_gains, capital_losses and divident from stocks - how it is distributed

  country mother, father, self have NA's and many levels - what should we do with it

  class of worker - Too many Not in universe and too many levels
  member of labour union , reason_for_unemployment, region_of_previous_residence - just too many values in Not in universe
  state_of_previous_residence seems equally distributed and 708 NA's and many Not in universe

  member of labour union - can not in universe be classified as No?
  hispanic origin - NA's can be classified as All Others
  migration_msa, migration_reg, migration_within_reg, migration_sunbelt - largne no of NA's should we treat as Not in Universe?
  family_members_under_18 - should we treat Not in universe as neither parent present?
  live_1_year_old - Not in Universe seems like No
  fill questionare for vetern - Not in universe may be No and for now 3 levels

  industry code, occupation code - too many levels and not represented as a factor
  major industry code, major occupation code, education - many levels

  veterans_benefits seems like 0 or 2 but not a factor
  business or self employed - factor [0,2]

  weeks worked in a year 0 to 52 but can be binned?

  year has either 94 or 95 is it relevent?
"
dim(train)
str(train)
summary(train)
describe(train)
rm(train)


# Impute Data - NA's 

# Deal with NA's in numeric data. 
# In this case we shall use median as the average will not be correct as the distribution is skewed
for(i in namesOfNumeric) {
  trainNumeric[,i] <- if_else(is.na(trainNumeric[,i]), median(trainNumeric[,i], na.rm=TRUE), trainNumeric[,i])
}

for(i in namesOfFactors) {
  trainFactors[,i] <- if_else(is.na(trainFactors[,i]), "Unavailable", as.character(trainFactors[,i]))
  trainFactors[,i] <- as.factor(trainFactors[,i])
}

# Have a better understanding of the data

numericPlot <- function(a){
  ggplot(data = trainNumeric, aes(x = trainNumeric[,a], y=..density..)) + geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) + geom_density()
}

# If needed add the target variable for analysis
catPlot <- function(a) {
  ggplot(trainFactors,aes(x=a, fill = target)) + geom_bar(position = "dodge") + 
    theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10))
}

# Group wage_per_hour, capital_gains, capital_losses, dividend_from_stocks with two levels yes and no
# Need more analysis to group ages
# Group weeks worked into 4 groups - did not work, worked 1-20, 20-40,40+ weeks 

#ggplot(data = trainNumeric, aes(x = trainNumeric$age, y = ..density..)) + geom_histogram(fill="light blue", color = "blue", bins = 100) 
#ggplot(data = trainNumeric, aes(x = trainNumeric$wage_per_hour)) + geom_histogram(fill="light blue", color = "blue", bins = 100) 
#ggplot(data = trainNumeric, aes(x = trainNumeric$capital_gains)) + geom_histogram(fill="light blue", color = "blue") 
#ggplot(data = trainNumeric, aes(x = trainNumeric$capital_losses)) + geom_histogram(fill="light blue", color = "blue") 
#ggplot(data = trainNumeric, aes(x = trainNumeric$dividend_from_Stocks)) + geom_histogram(fill="light blue", color = "blue")
#ggplot(data = trainNumeric, aes(x = trainNumeric$num_person_Worked_employer)) + geom_histogram(fill="light blue", color = "blue")
#ggplot(data = trainNumeric, aes(x = trainNumeric$weeks_worked_in_year)) + geom_histogram(fill="light blue", color = "blue", bins = 5)

# Group age as 0-15, 15-70, 70+
#ggplot(data=trainNumeric,aes(x = age, y=wage_per_hour)) + 
#  geom_point(colour = "light blue") + 
#  scale_y_continuous("wage per hour", breaks = seq(0,10000,1000)) + 
#  scale_x_continuous(breaks = seq(0,100,10))


# Manipulate Data 

# group levels that have less than or equal to 5% propotion and retain the category mapping
# include target for analysis
groupedFactorMap <- data.frame()
for(i in namesOfFactors) {
  groupByFactor <- group_by(trainFactors, trainFactors[,i])
  groupByFactor <- summarise(groupByFactor, f = n()/size)
  groupByFactor <- data.frame(filter(groupByFactor, groupByFactor[,2] <= 0.05))
  levels(trainFactors[,i])[levels(trainFactors[,i]) %in% groupByFactor[,1]] <- "Other"
  f <- i
  for(j in groupByFactor[,1]) {
    k <- j
    v <- "Others"
    groupedFactorMap <- bind_rows(groupedFactorMap, data.frame(f, k, v))
  }
  rm(groupByFactor)
}
summary(trainFactors)

# remove correlated variables
corCol <- findCorrelation(x = cor(trainNumeric), cutoff = 0.7)
correlatedVariables <- names(trainNumeric)[corCol] 
trainNumeric <- trainNumeric[,-corCol]

# Group numeric variables to categorical as decided earlier

# Group age as 0-15, 15-70, 70+
trainNumeric$age <- cut(trainNumeric$age, breaks = c(0,15,70,100), include.lowest = TRUE, labels = c("young","adult","old"))
trainNumeric$age <- as.factor(trainNumeric$age)

# Group wage_per_hour, capital_gains, capital_losses, dividend_from_stocks with two levels yes and no
colsForTwoLevels <- c("wage_per_hour", "capital_gains", "capital_losses", "dividend_from_Stocks")
for(i in colsForTwoLevels) {
  trainNumeric[,i] <- if_else(trainNumeric[,i] <=0, "Zero", "MoreThanZero")
  trainNumeric[,i] <- as.factor(trainNumeric[,i])
}
summary(trainNumeric)

trainNumeric <- trainNumeric[,-7]
train <- bind_cols(trainFactors, trainNumeric)
rm(trainNumeric)
rm(trainFactors)

# Classification using CART
sizeOfTheDataSet <- nrow(train)
sizeOfTheDataSet

minBucketSize <- 0.3*sizeOfTheDataSet/100 
minBucketSize

minSplitSize <- 2*minBucketSize  
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

# prune the tree
classificationModel <- prune(classificationModel, cp = 0.0019 ,"CP")
printcp(classificationModel)
fancyRpartPlot(classificationModel)

# predict the target for the training sample
pTarget <- predict(classificationModel, train, type="class")

confusionMatrix(pTarget, train$target, positive = "1")
names(train)
# random forest
library(randomForest)
rfModel <- randomForest(formula = target ~ ., data = train)
rfModel
confusionMatrix(rfModel$predicted, train$target, positive = "1")

"
# try out xg boost
library(xgboost)
library(Matrix)
library(mlr)
library(dummies)

train <- train[,-c(41,42)]
train <- dummy.data.frame(train[,-c(34)], sep = "")
train[] <- lapply(train, as.numeric)
train$target <- as.factor(train$target)
names(train) <- make.names(names = names(train), unique = TRUE)
names(train)
"

#sparse_matrix <- sparse.model.matrix(target ~ .-1, data = train)
#dim(sparse_matrix)
#target <- train$target
#target <- as.integer(target) - 1

#train.task <- makeClassifTask(data = train, target = "target")

#remove zero variance features
#train.task <- removeConstantFeatures(train.task)

#set.seed(2002)
#xgbLearner <- makeLearner("classif.xgboost", predict.type = "response")
#xgbLearner$par.vals <- list(
#                      objective = "binary:logistic",
#                      eval_metric = "error",
#                      nrounds = 150
#)
#define hyperparameters for tuning
#xg_ps <- makeParamSet( 
#  makeIntegerParam("max_depth",lower=3,upper=10),
#  makeNumericParam("lambda",lower=0.05,upper=0.5),
#  makeNumericParam("eta", lower = 0.01, upper = 0.5),
#   makeNumericParam("subsample", lower = 0.50, upper = 1),
#  makeNumericParam("min_child_weight",lower=2,upper=10),
#  makeNumericParam("colsample_bytree",lower = 0.50,upper = 0.80)
#)

#define search function
#rancontrol <- makeTuneControlRandom(maxit = 5L) #do 5 iterations

#5 fold cross validation
#set_cv <- makeResampleDesc("CV",iters = 5L,stratify = TRUE)

#xgb_tune <- tuneParams(learner = xgbLearner, 
#                       task = train.task, 
#                       resampling = set_cv, 
#                       measures = list(acc,tpr,tnr,fpr,fp,fn), 
#                       par.set = xg_ps, 
#                       control = rancontrol)

# xgb_new <- setHyperPars(learner = xgbLearner, par.vals = xgb_tune$x)

# xgmodel <- train(xgb_new, train.task)

# myAlgo <- xgboost(data = sparse_matrix,
#        label = target,
#        eta = 0.1,
#        max_depth = 15, 
#        nrounds = 25, 
#        subsample = 0.5,
#        colsample_bytree = 0.5,
#        seed = 1,
#        eval_metric = "error",
#        objective = "binary:logistic",
#        nthread = 3)


#y_pred <- predict(xgmodel, train.task)

#length(y_pred)
#y_pred <- as.numeric(y_pred > 0.5)
#imp_matrix <- xgb.importance(feature_names = colnames(sparse_matrix), model = myAlgo)
#print(imp_matrix)
#pred2 <- setThreshold(y_pred,0.4)
#confusionMatrix(y_pred$data$response, train$target)
