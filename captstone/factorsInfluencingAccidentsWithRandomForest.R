library(dplyr)
library(randomForest)
library(car)

accidents <- read.csv('./largeDataSets/capstone - accident/consolidatedAccidentData.csv')

accidents <- accidents %>%
  dplyr::mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
  dplyr::filter(Date > as.Date("30/12/2013", "%d/%m/%Y"))

accidents <- accidents %>% 
  dplyr::select(
    -X...Accident_Index,
    -Location_Easting_OSGR,
    -Location_Northing_OSGR,
    -Longitude,
    -Latitude,
    -Date,
    -Time,
    -Time_In_Hrs,
    -no_of_fatal_casualties,
    -no_of_serious_casualties,
    -no_of_fatalserious_casualties,
    -no_of_slight_casualties,
    -LSOA_of_Accident_Location,
    -Local_Authority_.District.,
    -Local_Authority_.Highway.,
    -X1st_Road_Number,
    -X2nd_Road_Number
    )

accidents <- accidents %>%
  dplyr::mutate(Accident_Severity = if_else(Accident_Severity %in% c(1,2), 1, 0))

for(i in c(1:2, 5:21)) {
  accidents[,i] <- as.factor(accidents[,i]) 
}

rfModel <- randomForest(accidents$Accident_Severity~.,
                        data = accidents[,-2],
                        ntree = 500,
                        mtry = 10, 
                        nodesize = 10000,
                        importance = TRUE)

rfModel
plot(rfModel)

