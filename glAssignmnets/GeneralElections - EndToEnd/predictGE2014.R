library(caTools)
library(randomForest)
library(dplyr)
library(ggplot2)
library(pROC)
library(caret)

candidates2009 <- read.csv("./glAssignmnets/GeneralElections - EndToEnd/candidatesIn2009.csv")
candidates2014 <- read.csv("./glAssignmnets/GeneralElections - EndToEnd/candidatesIn2014.csv")

candidates2009$seat_won <- as.factor(candidates2009$seat_won)

# Split the data into dev & hOut
set.seed(9090)
split <- sample.split(candidates2009$seat_won, SplitRatio = 0.70)
test <- subset(candidates2009, split == TRUE)
train <- subset(candidates2009, split == FALSE)

# create the random forest model & logistic regression model
f <- formula(seat_won~education+assets+seriousCrimes+notSeriousCrimes+gender)
fit1 <- randomForest(f, 
                     data = test,
                     ntree = 100,
                     mtry = 3,
                     importance = TRUE)

pSeatWon <- predict(fit1, test, type = "class") 
confusionMatrix(pSeatWon, test$seat_won, positive = "1")

probSeatWon <- predict(fit1, test, type = "prob")  %>% data.frame()
auc <- roc(test$seat_won,probSeatWon$X1)
auc
plot(auc)

ggplot(probSeatWon) + 
  geom_histogram(aes(probSeatWon$X0, fill = "Red")) + 
  geom_histogram(aes(probSeatWon$X1, fill = "Light Blue"))

predicted <- predict(fit1, candidates2014, type = "prob")
candidates2014$pSeat_Won = if_else(predicted[,2] >= 0.4, 1, 0)

summary <- 
  candidates2014 %>% 
  dplyr::group_by(party) %>% 
  dplyr::summarise(seats_won = sum(seat_won), p_seats_won = sum(pSeat_Won)) %>%
  dplyr::arrange(desc(p_seats_won)) %>% 
  dplyr::top_n(10) %>%
  data.frame()

summary

ggplot(summary) +
  geom_bar(aes(x = party, y = p_seats_won), width = 2, stat = "identity") + 
  coord_polar("y", start=0)

ggplot(summary) +
  geom_bar(aes(x = "", y = p_seats_won, fill = party), width = 2, stat = "identity") + 
  coord_polar("y", start=0)
