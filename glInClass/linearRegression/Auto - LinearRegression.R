library(readxl)
library(car)

# Import Auto.xlsx
Auto <- read_excel("./glInClass/linearRegression/Auto.xlsx")
View(Auto)

# Pairwise correlation coefficients
round(cor(Auto),2)

# Regress mpg on the predictors in the data set
a.fit1 <- lm(mpg ~ ., data=Auto) 

summary(a.fit1)
anova(a.fit1)

# variance inflation factor
round(vif(a.fit1),2) 

# linear regression model
a.fit2 <- lm(mpg ~   cylinders + displacement + weight + acceleration, data=Auto)
summary(a.fit2)
anova(a.fit2)

# variance inflation factor
round(vif(a.fit2),2)

# Exclude the variable that thas the highest variance inflation factor
a.fit3 <- lm(mpg ~ displacement + weight + acceleration, data=Auto)
summary(a.fit3)
round(vif(a.fit3),2)

a.fit4 <- lm(mpg ~ weight  + acceleration, data=Auto)
summary(a.fit4)
round(vif(a.fit4),2)

# Predict using the dependent variables
AutoTest <- read_excel("./glInClass/linearRegression/AutoTest.xlsx")
View(AutoTest)

a.fit4.prediction <- predict(a.fit4, AutoTest, interval = "prediction")
a.fit4.prediction

# Plot the predicted and actual value to check for accuracy of prediction
predictionAndActuals <- data.frame(a.fit4.prediction[,1], AutoTest$mpg)
colnames(predictionAndActuals) <- c("Predicted mpg","Actual mpg")
matplot(predictionAndActuals, type = "l")
predictionAndActuals
matplot(a.fit4.prediction, type = "l")

a.fit4.confidenceForMean <- predict(a.fit4, AutoTest, interval = "confidence")
a.fit4.confidenceForMean
matplot(a.fit4.confidenceForMean, type = "l")
