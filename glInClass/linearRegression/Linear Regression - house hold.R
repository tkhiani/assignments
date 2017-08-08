## Read Consumer data

library(psych)

## Summary Statistics
describe(Consumer)
cor(Consumer)

with(Consumer, boxplot(Income, main="Income (1000) US$"))
with(Consumer, boxplot(HouseholdSize, main="Household Size"))
with(Consumer, boxplot(AmountCharged, main="Amount Charged US$"))

with(Consumer, plot(HouseholdSize, AmountCharged, pch=19, cex=0.6))
with(Consumer, plot(Income, AmountCharged, pch=19, cex=0.6))

## Normality Test
with(Consumer, shapiro.test(Income))
with(Consumer, qqnorm(Income, pch=19, cex=0.6))
with(Consumer, qqline(Income, col='red'))

## Simple Regressions
reg1 <- lm(AmountCharged ~ Income, data=Consumer)
reg1
summary(reg1)
anova(reg1)

reg2 <- lm(AmountCharged ~ HouseholdSize, data=Consumer)
reg2
summary(reg2)
anova(reg2)

## Multiple Regression
reg3 <- lm(AmountCharged ~ Income + HouseholdSize, data=Consumer)
reg3
summary(reg3)
anova(reg3)

fit3 <- fitted(reg3)
res3 <- residuals(reg3)

abline(reg1, col='red')
abline(reg2, col='red')

ConsumerReg <- cbind(Consumer, fit3, res3)

## Prediction of new observations
newobs <- data.frame(Income = 40, HouseholdSize = 3)
predict.lm(reg3, newdata=newobs)

# newobs <- data.frame(Income = c(40,50), HouseholdSize = c(3, 4))
# predict.lm(reg3, newdata=newobs)

regDS <- lm(Medical1$`Depression Score`~Medical1$'Place')
summary(regDS)

regP1 <- lm(Promotion$Sales ~ Promotion$`In-Store Promotion`)
summary(regP1)

####### Reading Brand data 
with(Brand, plot(Score, Price, pch=19, col='blue'))
with(Brand, cor(Score, Price))

regB <- lm(Price ~ Score, data=Brand)
summary(regB)
anova(regB)
