mydata <- read.csv("./discriminateAnalysis/discri-nm1.csv")
mydata <- mydata[,-c(1,8)]
mydata$visit <- as.factor(mydata$visit)

summary(mydata)

attach(mydata)
by(mydata, INDICES=visit, FUN=summary)
boxplot(income~visit, col=c("Red","Pink", "Green"), main="Pattern Revealed By Community", ylab ="Price")
boxplot(travel~visit, col=c("Red","Pink", "Green"), main="Pattern Revealed By Community", ylab ="Bedrooms")
boxplot(vacation~visit,col=c("Red","Pink", "Green"), main="Pattern Revealed By Community", ylab ="SquareFootage")
boxplot(hsize~visit,col=c("Red","Pink", "Green"), main="Pattern Revealed By Community", ylab ="SquareFootage")
boxplot(age~visit,col=c("Red","Pink", "Green"), main="Pattern Revealed By Community", ylab ="SquareFootage")

library(DiscriMiner)
X1=mydata[, -1]
Y1=mydata[, 1]

library(psych)
describeBy(X1,group=visit)
mymodel=linDA(X1, Y1)
mymodel
mymodel$scores
cor(mymodel$scores,X1)
data3=data.frame(mymodel$scores)
data3
attach(data3)
Prob1=exp(1)/(exp(1)+exp(2))
Prob2=exp(2)/(exp(1)+exp(2))
PosteriorProbability=data.frame(Prob1, Prob2)
PosteriorProbability
X1=cbind(as.matrix(mydata[, -1]))
Manova=manova(X1~Y1)
summary(Manova)
summary(Manova, test="Wilks")
summary.aov(Manova)
discPower(X1, Y1) 

# work with lda for cross validation
m <- lda(Y1~X1, data = mydata, cv = TRUE)
m
