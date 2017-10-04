# Inspiration: 
  # https://www.r-bloggers.com/implementing-apriori-algorithm-in-r/
  # https://www.analyticsvidhya.com/blog/2017/08/mining-frequent-items-using-apriori-algorithm/

library(dplyr)
library(plyr)
library(stringi)

transactions <- read.csv("./largeDataSets/cafe-assignment-marketingAndRetailAnalysis/Cafe Great Transaction Data.csv")

head(transactions)
str(transactions)

transactions <- transactions %>%
  dplyr::mutate(
    Bill.Number = paste(1, stri_sub(Bill.Number, 2), sep = ""),
    Bill.Number = as.numeric(Bill.Number),
    Item.Desc = gsub(pattern = "\207\207\207\207\207\207\207\207", replacement = "", Item.Desc),
    Item.Desc = gsub(pattern = " ", replacement = "", Item.Desc),
    Date = as.Date(Date, "%d-%B-%y")) %>%
  dplyr::arrange(Bill.Number)

lastTransactionDate = max(transactions$Date)
firstTransactionDate = min(transactions$Date)

itemMinPrice <- transactions %>% 
  dplyr::group_by(Item.Desc) %>% 
  dplyr::filter(Rate == min(Rate)) %>%
  dplyr::summarise(minRate = max(Rate), dateOfMinRate = max(Date)) %>%
  data.frame()

itemMaxPrice <- transactions %>% 
  dplyr::group_by(Item.Desc) %>% 
  dplyr::filter(Rate == max(Rate)) %>%
  dplyr::summarise(maxRate = max(Rate), dateOfMaxRate = max(Date)) %>%
  data.frame()

itemWithMinMaxPrice <- join(itemMinPrice, itemMaxPrice, by = "Item.Desc") %>%
  dplyr::filter(minRate != maxRate) %>%
  dplyr::filter(dateOfMaxRate != dateOfMinRate) %>%
  dplyr::mutate(
    increaseInPrice = if_else(dateOfMaxRate > dateOfMinRate, 1, 0),
    priceChangeDate = if_else(increaseInPrice == 1, dateOfMaxRate, dateOfMinRate),
    startWindow = priceChangeDate - 15,
    endWindow = priceChangeDate + 15)

transactionsWithMinMaxPrice <- inner_join(transactions, itemWithMinMaxPrice, by = "Item.Desc")

rateChanges <- transactionsWithMinMaxPrice %>%
  dplyr::group_by(Item.Desc) %>% 
  dplyr::summarise(
    dateOfMaxRate = max(dateOfMaxRate),
    maxRate = max(maxRate), 
    minRate = min(minRate),
    dateOfMinRate = max(dateOfMinRate),
    priceChangeDate = max(priceChangeDate),
    startWindow = max(startWindow),
    endWindow = max(endWindow),
    soldAfter15DaysOfPriceChange = sum(if_else(Date >= priceChangeDate & Date < endWindow, Quantity, 0L)),
    soldBefore15DaysOfPriceChange = sum(if_else(Date >= startWindow & Date < priceChangeDate, Quantity, 0L)),
    soldAfterPriceChange = sum(Date >= priceChangeDate),
    soldBeforePriceChange = sum(Date < priceChangeDate),
    n = n(),
    numberOfDaysAfterPriceChange = as.numeric(lastTransactionDate - priceChangeDate + 1),
    numberOfDaysBeforePriceChange = as.numeric(priceChangeDate - firstTransactionDate),
    avgSoldAfterPriceChange = soldAfterPriceChange/numberOfDaysAfterPriceChange,
    avgSoldBeforePriceChange = soldBeforePriceChange/numberOfDaysBeforePriceChange,
    difference = (avgSoldAfterPriceChange - avgSoldBeforePriceChange), 
    increaseInPrice = max(increaseInPrice),
    increaseInQuanity = if_else(avgSoldAfterPriceChange > avgSoldBeforePriceChange, 1, 0),
    class = if_else(increaseInPrice == 1 & difference >= 0, "Increase in Price & Quanity",
                    if_else(increaseInPrice == 0 & difference >= 0, "Decrease in Price but Increase in Quanitity",
                            if_else(increaseInPrice == 0 & difference < 0, "Decrease in Price & Quanitity",
                                    "Increase in Price but decrease in Quanitity")))
  ) %>%
  dplyr::arrange(desc(difference)) %>%
  dplyr::filter(numberOfDaysAfterPriceChange > 14)
  data.frame() 

  write.csv(rateChanges,"./largeDataSets/cafe-assignment-marketingAndRetailAnalysis/rateChanges.csv", row.names = TRUE)
  
# To-Do: How to determine or classify where sales have increased or decreased.
# One simple option is to look at the latest price change and then express results 
# but let us disucss this in the group