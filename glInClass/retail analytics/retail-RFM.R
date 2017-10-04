# RFM (Max no. of segments 10)
#   Recency - 
#   Frequency - 
#   Monetory - 

library(dplyr)

retailData <- read.csv("./largeDataSets/retail-RFM/Retail_DataSet_Assign_Great_Lakes_loyalty.csv")

retailData <- retailData %>% 
  dplyr::filter(retailData$Loyal_card_flag == 1) %>%
  dplyr::group_by(Cust_ID) %>%
  dplyr::summarise(
    recency = max(Month), 
    frequency = n(),
    monetry = sum(Amount)
  ) %>%
  dplyr::mutate(
    recency_bin = if_else(recency == 12, 'H', if_else(recency %in% c(10, 11), 'M', 'L')),
    frequency_bin = if_else(frequency > 30, 'H', if_else(frequency > 10, 'M', 'L')),
    monetry_bin = if_else(monetry > 10000, 'H', if_else(monetry > 5000, 'M', 'L')),
    rfm = paste(recency_bin, frequency_bin, monetry_bin)
  ) %>%
  dplyr::arrange(desc(monetry)) %>%
  data.frame()

noOfRows <- nrow(retailData)
# Monetory, Frequency, Recency

retailData %>% 
  dplyr::group_by(rfm) %>% 
  dplyr::summarise(noOfCustomers = n()) %>% 
  dplyr::mutate(perOfTotal = noOfCustomers * 100 / noOfRows) %>%
  dplyr::arrange(desc(noOfCustomers)) %>%
  data.frame()
