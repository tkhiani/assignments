View(Dell_Data_Q10)

Q10 <- Dell_Data_Q10

round(var(Q10),2)
round(cor(Q10),2)

SQ10 <- scale(Q10)
SQ10 <- data.frame(SQ10)

# Principal Component Analysis #
fit.Q10.1 <- prcomp(Q10, scale=T) # Scaling recommended

plot(fit.Q10.1, type="l", main="Scree Plot of Dell Data Q10", col="blue")

names(fit.Q10.1)
summary(fit.Q10.1)
fit.Q10.1

# Factor Analysis #
library(psych)

fit.Q10.2 <- principal(Q10, nfactors=4, rotate="none")
fit.Q10.2

fit.Q10.3 <- principal(Q10, nfactors=4, rotate="varimax")
fit.Q10.3



