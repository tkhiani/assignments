library(caret)
library(Deducer)
library(pscl)

trainAll <- read.csv("./MailFromPaulGreen/PaulBooks1.csv")
test <- read.csv("./MailFromPaulGreen/PaulBooks2.csv")

# Split the data into train & hold_out
train <- sample_frac(trainAll, 0.7) 
hold_out <- trainAll[-as.numeric(rownames(train)),]

rm(trainAll)

glm <- glm(Purchase~Months+NoBought, train, family = "binomial")
summary(glm)

pR2(glm)

confint(glm)
proForTrain <- predict(glm, type="response")
pForTrain <- ifelse(proForTrain<=0.5, "0", "1")
confusionMatrix(pForTrain, train$Purchase, positive = "1")
rocplot(glm)

pForTrain <- ifelse(proForTrain<=(1/7), "0", "1")
confusionMatrix(pForTrain, train$Purchase, positive = "1")

probForHoldOut <- predict(glm, newdata = hold_out[,-c(1,4)], type = "response")
pForHoldOut <- ifelse(probForHoldOut<=(1/7), "0", "1")
confusionMatrix(pForHoldOut, hold_out$Purchase, positive = "1")

probForTest <- predict(glm, newdata = test[,-c(1,4)], type = "response")
pForTest <- ifelse(probForTest<=(1/7), "0", "1")
confusionMatrix(pForTest, test$Purchase, positive = "1")
