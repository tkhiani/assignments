View(SalesPersonData)

var(SalesPersonData)
cor(SalesPersonData)

SPData <- scale(SalesPersonData)
SPData <- data.frame(SPData)

M <- round(var(SPData),2)

# Principal Component Analysis #
fit.1 <- prcomp(SalesPersonData, scale=T) # Scaling recommended
SPData <- data.frame(SPData)names(fit.1)
summary(fit.1)
fit.1

round(cor(fit.1$x),2)
round(cor(SPData, fit.1$x),2)

plot(fit.1, type="l", main="Scree Plot of Sales Person Data", col="blue")

xl <- seq(-3, 3)
yl <- seq(-3, 3)
plot(xl, yl, type="n", xlab="", ylab="")
points(SPData$Height, SPData$Weight, pch=19, cex=0.8, col="blue", )
points(fit.1$x[,1], fit.1$x[,2], pch=10, cex=0.8, col="red")
title ("Comparison of Original Data and PC1, PC2")

# Factor Analysis #
library(psych)

fit.2 <- principal(SalesPersonData, nfactors=3, rotate="none")
fit.2

fit.3 <- principal(SalesPersonData, nfactors=3, rotate="varimax")
fit.3

fit.3$scores
