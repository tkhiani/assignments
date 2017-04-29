# load data
train <- read.csv("./data/BigMartSales/train.csv")
train$type <- 0
test <- read.csv("./data/BigMartSales/test.csv")
test$type <- 1
testItemIdentifier <- test$Item_Identifier
testOutletIdentifier <- test$Outlet_Identifier
salesData <- bind_rows(train, test)
head(salesData)

# Data Exploration 

# Summary to understand the summary statistics
# Some values are unknown - Item Weight, Visibility, Outlet Size
# Item Type has too many types/categories
# Item Fat Content can be further grouped as LF, Low Fat and low fat are the same
# Item Identifier and Outlet Identifier are unique can be kept out of the variables
summary(salesData)

# Determine the number of distinct values
# Determine the no. of NA's per variable?
library("dplyr")
summarise_all(salesData, funs(n_distinct(.)))

# Variables Item Weight, Item Visibility, Item Outlet Sales, Outlet Type - High positive skew
library("psych")
describe(salesData)

# Data Cleaning

# Impute the missing values
salesData <-mutate(salesData, Item_Weight = if_else(is.na(Item_Weight), mean(salesData$Item_Weight, na.rm=TRUE), Item_Weight))
salesData <- mutate(salesData, Outlet_Size = if_else(Outlet_Size=="", "Outlet Unknown", as.character(salesData$Outlet_Size)))

# Feature Engineering

# As visibility 0 means no value lets impute it with the mean
salesData <- mutate(salesData, Item_Visibility = if_else(Item_Visibility == 0, 
                                                         mean(Item_Visibility, na.rm = TRUE),
                                                         Item_Visibility))

# Item Fat Content can be further grouped as LF, Low Fat and low fat are the same
salesData <- mutate(salesData, Item_Fat_Content = if_else( Item_Fat_Content %in% c("LF", "low fat", "Low Fat"), "Low Fat","Regular"))

# Years of operations instead of Establishment Year
library("lubridate")
salesData$yearsInOperations <-  year(Sys.Date()) - salesData$Outlet_Establishment_Year

# Consider combining Outlet_Types
# However as the mean sales are different it does not make sense to combine outlet types
salesByOutletType <- group_by(salesData, Outlet_Type)
salesByOutletType %>% summarise(mean(Item_Outlet_Sales, na.rm = TRUE))

# Explore the propotions for Item Type as there are too many types?
# If you want to do it for all the rows: http://stackoverflow.com/questions/24901061/in-r-how-do-i-compute-factors-percentage-given-on-different-variable
salesData <- mutate(salesData, category = ifelse(grepl("FD",Item_Identifier), "Food",
                                                 ifelse(grepl("DR",Item_Identifier), "Drinks",
                                                        "Non Comsumables")))
summary(salesData)
salesData$category <- as.factor(salesData$category)
salesData$Outlet_Size <- as.factor(salesData$Outlet_Size)
salesData$Item_Fat_Content <- as.factor(salesData$Item_Fat_Content)
head(salesData)
summary(salesData)

# get dummies for regression
library(dummies)
head(salesData)
item_identifier <- salesData[,1]
salesData <- dummy.data.frame(salesData[,-c(1,7,5,8)], sep = ".")
summary(salesData)

#
tmpOutletSales <- salesData$Item_Outlet_Sales
tmpType <- salesData$type
salesData <- data.frame(scale(salesData[,-c(17,18)]))
salesData$Item_Outlet_Sales <- tmpOutletSales
salesData$type <- tmpType
head(salesData)
train <- filter(salesData[,-c(22)], salesData$type == 0)
test <- filter(salesData[,-c(22)], salesData$type == 1)

library("ridge")
model <- linearRidge(train$Item_Outlet_Sales~., train)
pValues <- predict(model, test)
head(pValues)

# club the classified result with the production data
results <- data.frame(Item_Identifier = testItemIdentifier, Outlet_Identifier = testOutletIdentifier, Item_Outlet_Sales = pValues)
View(results)
write.csv(results, file = "./assignment details/BigMart-Results.csv")