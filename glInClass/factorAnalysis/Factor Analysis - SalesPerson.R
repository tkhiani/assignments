library(readxl)
library(psych)

# Import Auto.xlsx
SalesPersonData <- read_excel("./glInClass/factorAnalysis/SalesPersonData.xlsx")
View(SalesPersonData)

# Variables are highly correlated
cor(SalesPersonData)

# Scale data so that variance is 1
var(SalesPersonData)
SPData <- scale(SalesPersonData)
SPData <- data.frame(SPData)
round(var(SPData),2)

# Principal Component Analysis 
salesPersonPCs <- prcomp(SPData)
names(salesPersonPCs)

# No correation between the principal components
round(cor(salesPersonPCs$x),2)

# Correlation is strong between one PC and one variable
round(cor(SPData,salesPersonPCs$x),2)

# Determine the number of principal components, pick only those that have a variance of greater than 1
summary(salesPersonPCs)
salesPersonPCs
plot(salesPersonPCs, type="l", main="Scree Plot of Sales Person Data", col="blue")

# Factor Analysis
salesPersonFactorsWithNoRotation <- principal(SalesPersonData, nfactors=3, rotate="none")
salesPersonFactorsWithNoRotation

# Factor Analysis with Varimax roration - maximizes the sum of the variances of the squared loadings (squared correlations between variables and factors)
salesPersonFactorsWithVarimaxRotation <- principal(SalesPersonData, nfactors=3, rotate="varimax")

# This indicates that we club Height, Weight to Physical Appearence; Education, IQ to Knowledge Score; the rest to maturity score
# We club dimensions based on the factor scores
salesPersonFactorsWithVarimaxRotation
