library(dplyr)

loyaltyData <- read.csv("./largeDataSets/retail-RFM/Loyalty Data set.csv")
loyaltyData <- loyaltyData %>% filter(Loyal_card_flag == 1)

unique(loyaltyData$Product)

# map categories
