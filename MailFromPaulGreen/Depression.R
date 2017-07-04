library(caret)
library(Deducer)
library(pscl)
library(lmtest)

train <- read.csv("./MailFromPaulGreen/Depression.csv")

fFields <- c("SEX","EMPLOY","INCOMECAT","DEPRESSION","SEX_INVERSE")
for(i in fFields) 
  train[,i] <- factor(train[,i])

head(train)
glm <- step(glm(DEPRESSION~SEX+INCOMECAT+EMPLOY+INCOMECAT*EMPLOY, train, family = "binomial"))
glm1 <- glm(DEPRESSION~SEX_INVERSE, train, family = "binomial")
summary(glm1)
exp(coef(glm1))
glm2 <- glm(DEPRESSION~SEX, train, family = "binomial")
summary(glm2)
exp(coef(glm2))
# determine the interaction effert in r - find out how to do it
# female is 1 and male is 0
# income category 0 - high, 1 - low
# employ 0 - full time, 1 - not full time
summary(glm)
exp(coef(glm))

pR2(glm)

confint(glm)
proForTrain <- predict(glm, type="response")
pForTrain <- ifelse(proForTrain<=0.5, "0", "1")
confusionMatrix(pForTrain, train$Purchase, positive = "1")
rocplot(glm)

probForHoldOut <- predict(glm, newdata = hold_out[,-c(1,4)], type = "response")
pForHoldOut <- ifelse(probForHoldOut<=0.5, "0", "1")
confusionMatrix(pForHoldOut, hold_out$Purchase, positive = "1")