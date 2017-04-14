library(readxl)
library(psych)

" 
Questionnaires are designed to measure Customer Satisfaction and some of them have related questions on purpose to ensure correct answers
The idea is to identify or group ralated dimensions or questions

Dell Questionare
Q01. Like introducing new brands to friends
Q02. Provide people with information on many kinds of products
Q03. People ask me for information on many kinds of products
Q04. My friends think of me as a good source of information
Q05. I like to take chance
Q06. Buying non-vetted products is a wastage of money
Q07. If people would quit experimenting, we would get lot accomplished
Q08. I like to try new things
Q09. I try new brands before others
Q10. I like to experiment
Q11. Friends ask my opinion about computer related products
Q12. My friends ask for my advice computer related products
Q13. I tell friends what I think of computer related products
"

# Import questonare data
dellData <- read_excel("./data/Dell Data.xlsx")
View(dellData)

round(var(dellData),2)
round(cor(dellData),2)

# Principal Component Analysis, Scaling recommended
dellPCs <- prcomp(dellData, scale=T)  

# Determine the number of PCs that are sufficient in this case 4
summary(dellPCs)
plot(dellPCs, type="l", main="Scree Plot of Dell Data", col="blue")
biplot(dellPCs)

# Factor Analysis #
dellFactorsNoRotation <- principal(dellData, nfactors=4, rotate="none")
dellFactorsNoRotation
biplot(dellFactorsNoRotation)

"
Combine Q01 to Q04
Combine Q05, Q08, Q09, Q10
Combine Q06, Q07
Combine Q11 to Q13

Factor 1: Information Provider
  Q01. Like introducing new brands to friends
  Q02. Provide people with information on many kinds of products
  Q03. People ask me for information on many kinds of products
  Q04. My friends think of me as a good source of information

Factor 2: Innovator
Q05. I like to take chance
Q08. I like to try new things
Q09. I try new brands before others
Q10. I like to experiment


Factor 3: Opinion leader
Q06. Buying non-vetted products is a wastage of money
Q07. If people would quit experimenting, we would get lot accomplished


Factor 4: Wet blanket (negative towards innovation)
  Q11. Friends ask my opinion about computer related products
  Q12. My friends ask for my advice computer related products
  Q13. I tell friends what I think of computer related products
"
dellFactorsVarimaxRotation <- principal(dellData, nfactors=4, rotate="varimax")
dellFactorsVarimaxRotation
biplot(dellFactorsVarimaxRotation)

