library(dplyr)
library(caTools)

accidents <- read.csv('./largeDataSets/capstone - accident/consolidatedAccidentData.csv')

accidents2010To2015 <- accidents %>%
  dplyr::mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
  dplyr::filter(Date > as.Date("31/12/2009", "%d/%m/%Y"))

set.seed(9090)
split <- sample.split(accidents2010To2015$Accident_Severity, SplitRatio = 0.30)
subsetOfAccidentData2010To2015 <- subset(accidents2010To2015, split == TRUE)
write.csv(subsetOfAccidentData2010To2015, './largeDataSets/capstone - accident/consolidatedsubSetAccidentDataFor2010To2015.csv', row.names = FALSE)

write.csv(accidents2010To2015, './largeDataSets/capstone - accident/consolidatedAccidentDataFor2010To2015.csv', row.names = FALSE)
rm(accidents2010To2015, subsetOfAccidentData2010To2015)

accidents2014To2015 <- accidents %>%
  dplyr::mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
  dplyr::filter(Date > as.Date("31/12/2013", "%d/%m/%Y"))

write.csv(accidents2014To2015, './largeDataSets/capstone - accident/consolidatedAccidentDataFor2014To2015.csv', row.names = FALSE)
rm(accidents2014To2015)
