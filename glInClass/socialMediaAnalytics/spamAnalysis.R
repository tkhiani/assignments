# First Practical Example of Text Mining

# You should set your working directory at this stage

spam <- read.csv("./largeDataSets/socialMediaAnalytics/01_Spambase.csv")

# Dependant variable is "Spam"- Let us make it categorical

spam$Spam=as.factor(spam$Spam)

# Let us find out how many spam mails this dataset contains

table(spam$Spam)

# Building Logistic Regression Model

# Step 1- Base Line Accuracy

2788/4601

# Step 2- Building Model

spamModelLR=glm(Spam~.,data=spam,family="binomial")

# Step 3- Finding Accuracy 

spamPred=predict(spamModelLR,data=spam,type="response")

table(spam$Spam,spamPred>0.5)

(2666+1619)/4601

# Interpreting Results

# Building Classification Model

# Install packages if you do not have them
# install.packages("rpart")
# install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

spamCART=rpart(Spam~.,data=spam,method="class")

# Plotting CART

prp(spamCART,extra=2)

# Interpreting Results

# Building Random Forest Model

# Install packages if you do not have them
# install.packages("randomForest")

library(randomForest)

spamRF=randomForest(Spam~.,data=spam)

varImpPlot(spamRF)


# Interpretation

# End of Script
