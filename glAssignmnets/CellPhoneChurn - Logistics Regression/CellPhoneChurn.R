library(psych)
library(caTools)
library(Deducer)
library(dplyr)
library(caret)
library(Deducer)
library(corrplot)
library(ggplot2)
library(pscl)
library(ROCR)
library(pROC)
library(lmtest)

# Load employee attrition data
all <- read.csv("./glAssignmnets/CellPhoneChurn - Logistics Regression/GA - Data set - Cell phone.csv")
head(all)
all$Churn <- all$X...Churn
all$X...Churn <- NULL

# Exploratory Data Analysis

# Remove fields that are not relevant for classificaiton employee number and employee count
head(all)
dim(all)
str(all)
summary(all)
describe(all)
describeBy(all, group = all$Churn)

# Convert factor fields to factors
factorFields <- c("Churn", "ContractRenewal", "DataPlan")
numericFields <- c("AccountWeeks", "DataUsage", "CustServCalls", "DayMins", "DayCalls", "MonthlyCharge", "OverageFee", "RoamMins")

all$ContractRenewal <- if_else(all$ContractRenewal == "1", "0", "1")

for(i in factorFields) {
  all[,i] <- as.factor(all[,i])
  if(i != "Churn")
    print(ggplot() + 
            geom_bar(aes(y = ..count..,x =all[,i], fill = Churn), 
                     data = all, position = position_stack()) + xlab(i))
}


for(i in numericFields) {
  print(ggplot(data = all, aes(all[,i], fill = Churn)) + geom_histogram(bins = 20) + xlab(i))
}

# The following are correlated
# MonthlyCharge - DataUsage (78.0%), DayMins (57.0%) 
r <- round(cor(all[,numericFields]),2)
r
corrplot(r, order = "hclust", tl.col='black', tl.cex=.75) 
version
# Churn Rate - 14.49%
churnRate <- sum(all$Churn == "1") / nrow(all)
churnRate

# Split the data into dev & hOut
set.seed(9090)
split <- sample.split(all$Churn, SplitRatio = 0.70)
dev <- subset(all, split == TRUE)
hOut <- subset(all, split == FALSE)

rm(all)

# Create the logistics regression classification model
cGlm <- step(glm(Churn~., dev, family = "binomial"), trace = 1, scope = .~.^2) 
# Evaluate overall significance
lrtest(cGlm)

# Evaluate what % of the intercept only model explains churn
pR2(cGlm)

# Evaluate if the variables used in the regression equation are significant
summary(cGlm)
oods <- exp(coef(cGlm))
oods

# Evaluate performance for the dev sample
proForDev <- cGlm$fitted.values
pForDev <- ifelse(proForDev<=0.5, "0", "1")
confusionMatrix(pForDev, dev$Churn, positive = "1")
rocplot(cGlm)
pred <- prediction(proForDev, dev$Churn)
plot(performance(pred,"lift","rpp"), main="lift curve", colorize=T)
plot(performance(pred, measure = "acc"))

# Evaluate performance for the hold out sample
probForhOut <- predict(cGlm, newdata = hOut, type = "response")
pForhOut <- ifelse(probForhOut<=0.5, "0", "1")
confusionMatrix(pForhOut, hOut$Churn, positive = "1")
auchOut <- roc(hOut$Churn,probForhOut)
auchOut
plot(auchOut)
pred <- prediction(probForhOut, hOut$Churn)
plot(performance(pred,"lift","rpp"), main="lift curve", colorize=T)

