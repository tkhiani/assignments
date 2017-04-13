# Import Auto.xlsx #

library(car)
View(Auto)

round(cor(Auto),2)
# Pairwise correlation coefficients

a.fit1 <- lm(mpg ~ ., data=Auto) 
# Regress mpg on the predictors in the data set

summary(a.fit1)
anova(a.fit1)

round(vif(a.fit1),2) # variance inflation factor

a.fit2 <- lm(mpg ~   cylinders + displacement + weight + acceleration, data=Auto)
summary(a.fit2)
anova(a.fit2)

round(vif(a.fit2),2)

a.fit3 <- lm(mpg ~ displacement + weight + acceleration, data=Auto)
summary(a.fit3)
round(vif(a.fit3),2)

a.fit4 <- lm(mpg ~ weight  + acceleration, data=Auto)
summary(a.fit4)
round(vif(a.fit4),2)

AutoX <- Auto[,-1] # Predictor matrix

round(var(AutoX),2)

# Principal component analysis
a.pc.1 <- prcomp(AutoX, scale=T) # scaling recommended
plot(a.pc.1, type='l', main ="Scree Plot of Auto Data")
summary(a.pc.1)
a.pc.1$rotation

SAuto <- scale(Auto)
SAuto <- data.frame(SAuto)


xl <- seq(-3, 3)
yl <- seq(-3, 3)
plot(xl, yl, type="n", xlab="", ylab="")
points(SAuto$displacement, SAuto$horsepower, pch=19, cex=0.8, col="forestgreen")
points(SAuto$weight, SAuto$acceleration, pch=12, cex=0.6, col="blue")
points(a.pc.1$x[,1], a.pc.1$x[,2], pch=10, cex=0.8, col="red")
title ("Comparison of Original Data and PC Scores")

# Principal component regression
Auto1 <- cbind(Auto[,1], a.pc.1$x[,1], a.pc.1$x[,2])
Auto1 <- data.frame(Auto1)
a.fit5 <- lm(Auto1[,1] ~ Auto1[,2] + Auto1[,3])
summary(a.fit5)

vif(a.fit5)


