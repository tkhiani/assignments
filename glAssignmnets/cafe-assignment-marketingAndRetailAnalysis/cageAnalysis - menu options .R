# Inspiration: 
  # https://www.r-bloggers.com/implementing-apriori-algorithm-in-r/
  # https://www.analyticsvidhya.com/blog/2017/08/mining-frequent-items-using-apriori-algorithm/

library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(arules)
library(arulesViz)

# Read all transactions
transactions <- read.csv("./largeDataSets/cafe-assignment-marketingAndRetailAnalysis/Cafe Great Transaction Data.csv")

# Convert to date types and data cleaning
transactions <- transactions %>%
  dplyr::mutate(
    Bill.Number = paste(1, stri_sub(Bill.Number, 2), sep = ""),
    Bill.Number = as.numeric(Bill.Number),
    Item.Desc = gsub(pattern = "\207\207\207\207\207\207\207\207", replacement = "", Item.Desc),
    Item.Desc = gsub(pattern = " ", replacement = "", Item.Desc),
    Date = as.Date(Date, "%d-%B-%y")) %>%
  dplyr::arrange(Bill.Number)

# Work with transactions with more than one item
transactionsWithMoreThanOneItem <- transactions %>%
  dplyr::group_by(Bill.Number) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

transactionsWithMoreThanOneItem <- transactions %>%
  dplyr::filter(Bill.Number %in% transactionsWithMoreThanOneItem$Bill.Number)

identifyCombinations <- function(fileName, data) {
  # Convert items with more than one transaction in the format Bill No, Date, [item1,item2,...]
  transactionBasedOnItems <- plyr::ddply(
    data,
    c("Bill.Number","Date"),
    function(df1) paste(df1$Item.Desc, collapse = ","))
  
  transactionBasedOnItems$Bill.Number = NULL
  transactionBasedOnItems$Date = NULL
  colnames(transactionBasedOnItems) <- c("itemList")
  write.csv(transactionBasedOnItems, fileName, row.names = TRUE)
  
  # Identify which products go together for a menu suggestion
  tranx <- read.transactions(
    file = fileName,
    rm.duplicates = TRUE, format = "basket", sep = ",", cols = 1)
  
  summary(tranx)
  
  rules <- arules::apriori(tranx, 
                           parameter = list(supp = 0.001, conf = 0.001, target = "rules"))
  summary(rules)
  inspect(rules)
  
  plot(rules)
  itemFrequencyPlot(tranx, topN = 5)
}

# Work with transactions with more than one item & at least one of the items have food as a category
transactionWithFood <- transactionsWithMoreThanOneItem %>%
  filter(Category == 'FOOD')

# Work with transactions with more than one item & at least one of the items have merchandise as a category
transactionWithMerchanise <- transactionsWithMoreThanOneItem %>%
  filter(Category == 'MERCHANDISE')

# Work with transactions with more than one item & at least one of the items have merchandise as a category
transactionWithMisc <- transactionsWithMoreThanOneItem %>%
  filter(Category == 'MISC')

# Work with transactions with more than one item & at least one of the items have wines as a category
transactionWithWines <- transactionsWithMoreThanOneItem %>%
  filter(Category == 'WINES')

# Work with transactions with more than one item & at least one of the items have liquor as a category
transactionWithBeverage <- transactionsWithMoreThanOneItem %>%
  filter(Category == 'BEVERAGE')

identifyCombinations(
  "./largeDataSets/cafe-assignment-marketingAndRetailAnalysis/ItemList.csv", 
  transactionsWithMoreThanOneItem)

identifyCombinations(
  "./largeDataSets/cafe-assignment-marketingAndRetailAnalysis/ItemListWithFood.csv", 
  transactionWithFood)

identifyCombinations(
  "./largeDataSets/cafe-assignment-marketingAndRetailAnalysis/ItemListWithMerchandise.csv", 
  transactionWithMerchanise)

identifyCombinations(
  "./largeDataSets/cafe-assignment-marketingAndRetailAnalysis/ItemListWithWines.csv", 
  transactionWithWines)

identifyCombinations(
  "./largeDataSets/cafe-assignment-marketingAndRetailAnalysis/ItemListWithMisc.csv", 
  transactionWithMisc)

identifyCombinations(
  "./largeDataSets/cafe-assignment-marketingAndRetailAnalysis/ItemListWithBeverages.csv", 
  transactionWithBeverage)

