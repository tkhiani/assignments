library("psych")
library("dplyr")

dowJoesData <- read.csv("./data/DowJones.csv")
head(dowJoesData)

dowJoesData <- dowJoesData[,-1]
boxplot(dowJoesData)
sampleSize <- nrow(dowJoesData)
r <- cor(dowJoesData)

# Bartlett test, Ho: Variables are not correlated, Ha: Variables are correlated
# We can also use the KMO index, closer to 1 then correlated
# Justify for a dimension reduction technique

cortest.bartlett(r, n = sampleSize) 
KMO(r)

# As Variances of the indices are widely different scaling is needed
var(dowJoesData)
sDowJoesData <- scale(dowJoesData, scale = TRUE, center = TRUE)
var(sDowJoesData)

# Principal Component Analysis #
pcDowJoes <- prcomp(sDowJoesData)

# Determine the no of PCs that are sufficient
summary(pcDowJoes)
plot(pcDowJoes, type="l", main="Scree Plot", col="blue")


# Not sure how to interpret a biplot
biplot(pcDowJoes)

# Factor Analysis 
dowJoesVarimaxRotation6 <- principal(sDowJoesData, nfactors = 6, rotate = "varimax")
dowJoesVarimaxRotation7 <- principal(sDowJoesData, nfactors = 7, rotate = "varimax")
dowJoesVarimaxRotation6$loadings
dowJoesVarimaxRotation7$loadings

# CSCO: 6 factors are able to explain 70% (least) of the variability  
# UNH: 7 factors are able to explain 84% (least) of the variability. Hence choose a 7 factor model  
dowJoesVarimaxRotation7$communality
dowJoesVarimaxRotation7$communality

# It is also possible to model each index as a function of a subset of the factors.
# For example, if we agree to use a threshold cut-off at 70%


# Group 1 - MMM, AXP, BA(?), CVX, DD, XOM, JNJ, MRK, PG, UTX, WMT 
# Group 2 - T, KO, GE, HD, PFE(?), TRV, VZ, V, DIS 
# Group 3 - UNH, NKE, MCD, IBM
# Group 4 - CAT, CSCO(?), INTC, MSFT
# Group 5 - GS, JPM
# Group 6 - MCD
# Group 7 - CSCO