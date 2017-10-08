library(readxl)
library(dplyr)
library(psych)
library(scales)
library(caret)
library(pROC)
library(car)

companyData <- readxl::read_excel('./largeDataSets/finance-risk-analytics/raw-data.xlsx', 
                                  sheet = 'raw data') %>% as.data.frame()

companyData$`Deposits (accepted by commercial banks)` <- NULL

summary(companyData)
describe(companyData)
str(companyData)

for(i in 10:ncol(companyData)) {
  companyData[,i] <- as.numeric(companyData[,i])
}

for(i in 3:ncol(companyData)){
  companyData[is.na(companyData[,i]), i] <- mean(companyData[,i], na.rm = TRUE)
}

companyData <- companyData %>%
  dplyr::mutate(willDefault = if_else(`Networth Next Year` < 0, 1, 0)) %>%
  dplyr::select(
    -Num,
    -`Networth Next Year`
  )

numberOfColumns = ncol(companyData) - 1

for(i in 1:numberOfColumns) {
  quantile <- quantile(companyData[,i], c(.02, .98))
  companyData[,i] <- if_else(companyData[,i] < quantile[1], quantile[1], 
          if_else(companyData[,i] > quantile[2], quantile[2], companyData[,i]))
}

companyData$Sales <- log10(companyData$Sales)

set.seed(9090)
split <- caTools::sample.split(companyData$willDefault, SplitRatio = 0.70)
train <- subset(companyData, split == TRUE)
test <- subset(companyData, split == FALSE)

# Choose the variable based on categories Leverage, Profitability, Size, Liquidity

# Run the variables
logit1 <- glm(willDefault~
                `Debt to equity ratio (times)` + 
                `PBDITA as % of total income` + 
                Sales + 
                `Current ratio (times)`
                , train, family = 'binomial')
summary(logit1)
exp(coef(logit1))

# Determine the score for each record and rank order them
# The top 10 percentile should be 5 to 6 times the default rate
# The bottom 10 percentile should be less than the default rate

train <- train %>% 
  dplyr::mutate(
    score = logit1$coefficients[1] + 
      logit1$coefficients[2] * `Debt to equity ratio (times)` +
      logit1$coefficients[3] * `PBDITA as % of total income` +
      logit1$coefficients[4] * Sales +
      logit1$coefficients[5] * `Current ratio (times)`
  ) %>%
  arrange(desc(score))

train$decile <- ntile(train$score, 10)
train %>% 
  dplyr::filter(decile == 10 & willDefault == 1) %>% 
  summarise(n = n()) 


test <- test %>% 
  dplyr::mutate(
    score = logit1$coefficients[1] + 
      logit1$coefficients[2] * `Debt to equity ratio (times)` +
      logit1$coefficients[3] * `PBDITA as % of total income` +
      logit1$coefficients[4] * Sales +
      logit1$coefficients[5] * `Current ratio (times)`
  ) %>%
  arrange(desc(score))
test$decile <- ntile(test$score, 10)
test %>% 
  dplyr::filter(decile == 10 & willDefault == 1) %>% 
  summarise(n = n()) 



# Evaluate performance on the test sample
predictedProbabilityForTest <- predict(logit1, newdata = test, type = "response")
predictedSeverityForTest <- ifelse(predictedProbabilityForTest < 0.50, "0", "1")
confusionMatrix(predictedSeverityForTest, test$willDefault, positive = "1")
rocForTest <- roc(test$willDefault, predictedProbabilityForTest)
rocForTest
plot(rocForTest)
