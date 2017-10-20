library(readxl)

stockData <- readxl::read_excel('./largeDataSets/finance-risk-analytics/2 Stocks Analysis.xlsx', 
                                  sheet = 'Data') %>% as.data.frame()

meanForAsianPaints <- mean()
sdForAsianPaints <- sd()