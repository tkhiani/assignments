library(readxl)
library(car)

# Import Auto.xlsx
Auto <- read_excel("~/Documents/Tarun/Tarun@Personal/MyProjects/assignments/data/Auto.xlsx")
View(Auto)


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
