library(readxl)
library(dplyr)
library(psych)
library(scales)

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

quantile(companyData$`Total assets`, c(.02, .98))



for(i in 1:numberOfColumns) {
  quantile <- quantile(companyData[,i], c(.02, .98))
  companyData[,i] <- if_else(companyData[,i] < quantile[1], quantile[1], 
          if_else(companyData[,i] > quantile[2], quantile[2], companyData[,i]))
}

colnames(companyData)

lm_model <- glm(willDefault~., companyData, family = 'binomial')
