library(readxl)
library(car)

# Import Auto.xlsx
car_data <- read_excel("./glInClass/linearRegression/Auto.xlsx")
View(car_data)

# Correlation across variables
cor(car_data)

# Determine if we need to scale the data so that the variance is 1
var(car_data)

#Scale data
sCarData <- data.frame(scale(car_data))
var(sCarData)

car_lm <- lm(mpg~., car_data)

#conducting PCA 
car_pca = prcomp(sCarData[,-1])

#view PCA
summary(car_pca)

#understand the equation of the PC components 
car_pca$rotation

#scree Plot to look at the relationship between variables
xl <- seq(-3, 3)
yl <- seq(-3, 3)
plot(xl, yl, type="n", xlab="", ylab="")
points(sCarData$displacement, sCarData$horsepower, pch=19, cex=0.8, col="forestgreen")
points(sCarData$weight, sCarData$acceleration, pch=12, cex=0.6, col="blue")
points(car_pca$x[,1], car_pca$x[,2], pch=10, cex=0.8, col="red")
title ("Comparison of Original Data and PC Scores")

# train data
car_train = cbind(car_data$mpg,car_pca$x[,1],car_pca$x[,2])
car_train = as.data.frame(car_train)
names(car_train) = c("mpg","PC1","PC2")

#View train data
View(car_train)

#create linear regression model on train data
car_pca_lm = lm(mpg~., data = car_train)

#View lm model
car_pca_lm

#Summary lm model
summary(car_pca_lm)

# Durbin-Watson Statistic, D = 0 if there is a strong relationship of residual with time else it is close to 2, alternate there is a relationship with time
dwt(car_pca_lm)

# Normality test for residual, null hypothesis: normally distributed
shapiro.test(car_pca_lm$residuals)

# Does residual have equal variance between residual and every value of x
plot(car_train$PC1 ,car_pca_lm$residuals)
plot(car_train$PC2 ,car_pca_lm$residuals)

#Variance Analysis
vif(car_pca_lm)

#Prepare to be in a position to test the data
cylinders_pc1 <- car_pca$rotation[1,1]
cylinders_pc2 <- car_pca$rotation[1,2]
cylinders_mean <- mean(car_data$cylinders)
cylinders_sd <- sd(car_data$cylinders)

displacement_pc1 <- car_pca$rotation[2,1]
displacement_pc2 <- car_pca$rotation[2,2]
displacement_mean <- mean(car_data$displacement)
displacement_sd <- sd(car_data$displacement)

horsepower_pc1 <- car_pca$rotation[3,1]
horsepower_pc2 <- car_pca$rotation[3,2]
horsepower_mean <- mean(car_data$horsepower)
horsepower_sd <- sd(car_data$horsepower)

weight_pc1 <- car_pca$rotation[4,1]
weight_pc2 <- car_pca$rotation[4,2]
weight_mean <- mean(car_data$weight)
weight_sd <- sd(car_data$weight)

acceleration_pc1 <- car_pca$rotation[5,1]
acceleration_pc2 <- car_pca$rotation[5,2]
acceleration_mean <- mean(car_data$acceleration) 
acceleration_sd <- sd(car_data$acceleration)

# creating test data
AutoTest <- read_excel("./glInClass/linearRegression/AutoTest.xlsx")

# creating test data based on PCA scores
PC1 <- ( (AutoTest$cylinders-cylinders_mean)/cylinders_sd * cylinders_pc1 + 
        (AutoTest$displacement-displacement_mean)/displacement_sd * displacement_pc1 +
          (AutoTest$horsepower-horsepower_mean)/horsepower_sd * horsepower_pc1 +
            (AutoTest$weight-weight_mean)/weight_sd * weight_pc1 +
              (AutoTest$acceleration-acceleration_mean)/acceleration_sd * acceleration_pc1 )

PC2 <- ( (AutoTest$cylinders-cylinders_mean)/cylinders_sd * cylinders_pc2 + 
           (AutoTest$displacement-displacement_mean)/displacement_sd * displacement_pc2 +
             (AutoTest$horsepower-horsepower_mean)/horsepower_sd * horsepower_pc2 +
               (AutoTest$weight-weight_mean)/weight_sd * weight_pc2 +
                 (AutoTest$acceleration-acceleration_mean)/acceleration_sd * acceleratio_pc2 )

# PC1: -0.027740807	-3.197059656	2.335436269
PC1
# PC2: 1.847547323	1.185961153	0.140719857
PC2

#Predict values for test data
predicted_mpg = predict(car_pca_lm,data.frame(PC1, PC2))

#View predicted data
predicted_mpg
predictionAndActuals <- data.frame(predicted_mpg, AutoTest$mpg)
colnames(predictionAndActuals) <- c("Predicted mpg","Actual mpg")
predictionAndActuals
matplot(predictionAndActuals, type = "l")
