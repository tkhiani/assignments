library(readxl)
library(dplyr)
library(psych)
library(caTools)

companyData <- readxl::read_excel('./largeDataSets/finance-risk-analytics/assignment/raw-data.xlsx', 
                                  sheet = 'raw data') %>% as.data.frame()
summary(companyData)
describe(companyData)
str(companyData)

# As there is no data for this column
companyData$`Deposits (accepted by commercial banks)` <- NULL

# Convert all variables to numeric as it represents no liability or defferred tax. Mean may not be the right option
for(i in 3:51) {
  companyData[,i] <- as.numeric(companyData[,i])
}

# Replace NA with 0
for(i in 3:51){
  companyData[is.na(companyData[,i]), i] <- 0
}

# Classify default as Networth Next Year is < 0 
companyData <- companyData %>% 
  dplyr::mutate(default = if_else(`Networth Next Year` < 0, 1, 0))

# Cap & Floor the variables

for(i in 3:51) {
  quantiles <- quantile(companyData[,i], c(.05, .95))
  companyData[,i] <- if_else(companyData[,i] < quantiles[1], quantiles[1], 
                             if_else(companyData[,i] > quantiles[2], quantiles[2], 
                                     companyData[,i]))
}

# Split the data into test and train
set.seed(9090)
split <- sample.split(companyData$default, SplitRatio = 0.70)
train <- subset(companyData, split == TRUE)
test <- subset(companyData, split == FALSE)


# Identify the variables that represent each category leverage, profitability, size, liquidity
companyDataWithProfitabilityVariables <- train %>% 
  dplyr::select(
    `default`,
    PBDITA,
    `Profit after tax`,
    PBT,
    `Cash profit`,
    `PBDITA as % of total income`,
    `PBT as % of total income`,
    `PAT as % of total income`,
    `Cumulative retained profits`
  )
for(i in colnames(companyDataWithProfitabilityVariables[,-1])) {
  formulaeString <- paste('default~', '`',i,'`', sep = "")
  formulae <- as.formula(formulaeString)
  profitEquation <- glm(formula = formulae, data = companyDataWithProfitabilityVariables, family = "binomial")  
  print(formulaeString)
  print(summary(profitEquation))
}

companyDataWithSizeVariables <- train %>% 
  dplyr::select(
    `default`,
    `Total assets`,
    `Net worth`, 
    `Sales`,
    `Total capital`,
    `Reserves and funds`,
    `Shareholders funds`,
    `Capital employed`,
    `Net fixed assets`)
for(i in colnames(companyDataWithSizeVariables[,-1])) {
  formulaeString <- paste('default~', '`',i,'`', sep = "")
  formulae <- as.formula(formulaeString)
  sizeEquation <- glm(formula = formulae, data = companyDataWithSizeVariables, family = "binomial")  
  print(formulaeString)
  print(summary(sizeEquation))
}

companyDataWithLiquidityVariables <- train %>% 
  dplyr::select(
    `default`,
    `Quick ratio (times)`,
    `Current ratio (times)`,
    `Cash to current liabilities (times)`,
    `Creditors turnover`,
    `Debtors turnover`,
    `Finished goods turnover`,
    `WIP turnover`,
    `Raw material turnover`)
for(i in colnames(companyDataWithLiquidityVariables[,-1])) {
  formulaeString <- paste('default~', '`',i,'`', sep = "")
  formulae <- as.formula(formulaeString)
  liquidityEquation <- glm(formula = formulae, data = companyDataWithLiquidityVariables, family = "binomial")  
  print(formulaeString)
  print(summary(liquidityEquation))
}

companyDataWithLevergeVariables <- train %>% 
  dplyr::select(
    `default`,
    `Debt to equity ratio (times)`,
    `TOL/TNW`,
    `Total term liabilities / tangible net worth`,
    `Contingent liabilities / Net worth (%)`,
    `Borrowings`,
    `Current liabilities & provisions`,
    `Deferred tax liability`,
    `Total liabilities`,
    `Contingent liabilities`)
for(i in colnames(companyDataWithLevergeVariables[,-1])) {
  formulaeString <- paste('default~', '`',i,'`', sep = "")
  formulae <- as.formula(formulaeString)
  leverageEquation <- glm(formula = formulae, data = companyDataWithLevergeVariables, family = "binomial")  
  print(formulaeString)
  print(summary(leverageEquation))
}

# Run logistics with one variable that is significant and has the highest co-effecient value from each category
  # Profitability - `PBDITA as % of total income` 
  # Size - `Reserves and funds`
  # Liquidity - `Quick ratio (times)`
  # Leverage - `Total term liabilities / tangible net worth`
# credit equation = -1.9429490 
#                   - 0.0997095 * `PBDITA as % of total income` 
#                   - 0.0023890 * `Reserves and funds` 
#                   - 0.8465098 * `Quick ratio (times)` 
#                   + 0.6847675 * `Total term liabilities / tangible net worth`
creditEquation <- glm(formula = default~`PBDITA as % of total income`+
                        `Reserves and funds`+
                        `Quick ratio (times)`+
                        `Total term liabilities / tangible net worth`, 
                      data = train, family = "binomial")  
summary(creditEquation)

# Score & Rank order for train
train <- train %>% 
  dplyr::mutate(
    score = creditEquation$coefficients[1] + 
      creditEquation$coefficients[2] * `PBDITA as % of total income` +
      creditEquation$coefficients[3] * `Reserves and funds` +
      creditEquation$coefficients[4] * `Quick ratio (times)` +
      creditEquation$coefficients[5] * `Total term liabilities / tangible net worth`) %>%
  dplyr::arrange(desc(score)) %>%
  dplyr::mutate(decile = ntile(score, 10))

trainDefaultRate <- sum(train$default) * 100 / nrow(train) 
trainNoOfDefaulters <- sum(train$default)
# trainDefaultRate - 6.516 %
trainDefaultRate

# % of defaulters identified in the first two deciles - 56%, 18%
# Default rate for the first two deciles are 36%, 12%
train %>% 
  dplyr::group_by(decile) %>% 
  dplyr::summarise(
    n = n(),
    default = sum(default),
    defaultRate = default * 100 / n,
    perOfDefaulterIdentified = default * 100 / trainNoOfDefaulters) %>%
  dplyr::arrange(desc(decile)) %>%
  dplyr::mutate(cumulativeDefaultRate = cumsum(default)*100/trainNoOfDefaulters) %>%
  data.frame()

# Score & Rank order for test
test <- test %>% 
  dplyr::mutate(
    score = creditEquation$coefficients[1] + 
      creditEquation$coefficients[2] * `PBDITA as % of total income` +
      creditEquation$coefficients[3] * `Reserves and funds` +
      creditEquation$coefficients[4] * `Quick ratio (times)` +
      creditEquation$coefficients[5] * `Total term liabilities / tangible net worth`) %>%
  dplyr::arrange(desc(score)) %>%
  dplyr::mutate(decile = ntile(score, 10))

testDefaultRate <- sum(test$default) * 100 / nrow(test) 
testNoOfDefaulters <- sum(test$default)
# testDefaultRate - 6.591 %
testDefaultRate

# % of defaulters identified in the first two deciles - 63%, 17%
# Default rate for the first two deciles - 41.51%, 11.32%
test %>% 
  dplyr::group_by(decile) %>% 
  dplyr::summarise(
    n = n(),
    default = sum(default),
    defaultRate = default * 100 / n,
    perOfDefaulterIdentified = default * 100 / testNoOfDefaulters) %>%
  dplyr::arrange(desc(decile)) %>%
  dplyr::mutate(cumulativeDefaultRate = cumsum(default)*100/testNoOfDefaulters) %>%
  data.frame()

# Score & Rank order for the validation sample
validationSample <- readxl::read_excel('./largeDataSets/finance-risk-analytics/assignment/validation_data.xlsx', 
                                  sheet = 'valdata') %>% as.data.frame()

validationSample$default <- validationSample$`Default - 1`

# Convert all variables to numeric as it represents no liability or defferred tax. Mean may not be the right option
for(i in 3:52) {
  validationSample[,i] <- as.numeric(validationSample[,i])
}

# Replace NA with 0
for(i in 3:52){
  validationSample[is.na(validationSample[,i]), i] <- 0
}

# Cap & Floor the variables
for(i in 3:52) {
  quantiles <- quantile(validationSample[,i], c(.05, .95))
  validationSample[,i] <- if_else(validationSample[,i] < quantiles[1], quantiles[1], 
                             if_else(validationSample[,i] > quantiles[2], quantiles[2], 
                                     validationSample[,i]))
}

validationSample <- validationSample %>% 
  dplyr::mutate(
    score = creditEquation$coefficients[1] + 
      creditEquation$coefficients[2] * `PBDITA as % of total income` +
      creditEquation$coefficients[3] * `Reserves and funds` +
      creditEquation$coefficients[4] * `Quick ratio (times)` +
      creditEquation$coefficients[5] * `Total term liabilities / tangible net worth`) %>%
  dplyr::arrange(desc(score)) %>%
  dplyr::mutate(decile = ntile(score, 10))

validationSampleDefaultRate <- sum(validationSample$default) * 100 / nrow(validationSample) 
validationSampleNoOfDefaulters <- sum(validationSample$default)
# trainDefaultRate - 7.55%
validationSampleDefaultRate

# % of defaulters identified in the first two deciles - 50%, 11.43%
# Default rate for the first two deciles - 49.30%, 11.42%
validationSample %>% 
  dplyr::group_by(decile) %>% 
  dplyr::summarise(
    n = n(),
    default = sum(default),
    defaultRate = default * 100 / n,
    perOfDefaulterIdentified = default * 100 / validationSampleNoOfDefaulters) %>%
  dplyr::arrange(desc(decile)) %>%
  dplyr::mutate(cumulativeDefaultRate = cumsum(default)*100/validationSampleNoOfDefaulters) %>%
  data.frame()
