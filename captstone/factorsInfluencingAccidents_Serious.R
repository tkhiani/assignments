# Here we eliminate variables primarly based on the VIF Score one after another & also ignore variables where they signify unknown
# We do not run VIF as we have done so in version1_stage1
library(dplyr)
library(dummies)
library(caTools)
library(lmtest)
library(pscl)
library(car)
library(caret)
library(pROC)

options(max.print=2000)

accidents <- read.csv('./largeDataSets/capstone - accident/consolidatedsubSetAccidentDataFor2010To2015.csv')

# Remove variables that are not needed 
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
    -Police_Force,
    -X1st_Road_Number,
    -X2nd_Road_Number,
    -Did_Police_Officer_Attend_Scene_of_Accident)

colnames(accidents)

for(i in c(4:19)) {
  accidents[,i] <- as.factor(accidents[,i]) 
}

accidents <- accidents %>%
  dplyr::mutate(Accident_Severity = if_else(Accident_Severity %in% c(1,3), 0, 1))

Accident_Severity <- accidents$Accident_Severity
accidents <- dummy.data.frame(accidents[-1], sep = ".")
accidents$Accident_Severity <- as.factor(Accident_Severity)

colnames(accidents)

# Remove variables where we do not have unknowns as they will not help explain accident severity
accidents <- accidents %>% 
  dplyr::select(
    -`X2nd_Road_Class.-1`,
    -`Road_Surface_Conditions.-1`,
    -`Special_Conditions_at_Site.-1`,
    -`Junction_Control.-1`,
    -`Pedestrian_Crossing.Human_Control.-1`,
    -`Pedestrian_Crossing.Physical_Facilities.-1`,
    -`Carriageway_Hazards.-1`,
    -`Weather_Conditions.-1`,
    -no_of_veh_in_ageNotKnown,
    -no_veh_driveNotKnown,
    -no_with_impactNotKnown,
    -no_motocyle_unknowncc,
    -no_vehicleNotKnown,
    -no_of_ped_movementUnknown,
    -no_of_goodsvenunknownt_occupant,
    -no_of_casualties_in_ageband_NotKnown,
    -no_of_veh_ccNotKnown,
    -no_of_drivers_in_ageband_NotKnown,
    -no_travelling_notKnown,
    -no_veh_junctionNotKnown,
    -no_goodsVehicleUnknownWeight,
    -no_of_ped_PedLocationUnknown,
    -no_of_unknowncc_occupant
  )

# Split the data into test and train
# Oversample the number of serious accidents in the training sample
set.seed(9090)
split <- sample.split(accidents$Accident_Severity, SplitRatio = 0.70)
train <- subset(accidents, split == TRUE)

seriousAccidentsInTrain <- dplyr::filter(train, Accident_Severity == "1")
train <- rbind(train, seriousAccidentsInTrain)
train <- rbind(train, seriousAccidentsInTrain)
train <- rbind(train, seriousAccidentsInTrain)

test <- subset(accidents, split == FALSE)

# Free up memory
rm(accidents, seriousAccidentsInTrain, split)

# Remove variables that have a linear relationship as indicated by alias & their coeffecitents are also NA
train <- train %>% 
  dplyr::select(
    -Day_of_Week.7,
    -X1st_Road_Class.6,
    -Road_Type.9,
    -Speed_limit.70,
    -Junction_Detail.9,
    -Light_Conditions.7,
    -Urban_or_Rural_Area.2,
    -Day.5,
    -no_of_other_drivers,
    -no_of_pedestrains
  )

# Remove variables that have a VIF score greater than 5
train <- train %>% 
  dplyr::select(
    -Carriageway_Hazards.0,
    -no_of_emotorcycle_occupant,
    -Pedestrian_Crossing.Physical_Facilities.0,
    -Weather_Conditions.1,
    -no_veh_notNextToJunction,
    -no_veh_goingAheadOther,
    -Number_of_Vehicles,
    -no_of_car_occupant,
    -Special_Conditions_at_Site.0,
    -no_with_front_impact,
    -Junction_Detail.0,
    -no_car,
    -Road_Surface_Conditions.1,
    -Pedestrian_Crossing.Human_Control.0,
    -Number_of_Casualties,
    -Road_Type.6,
    -Junction_Control.4,
    -no_of_passengers,
    -no_riddenHorse,
    -no_cycles,
    -Light_Conditions.1,
    -no_of_125cc_occupant,
    -X2nd_Road_Class.6,
    -no_of_50cc_occupant,
    -no_of_male_drivers,
    -Speed_limit.30,
    -no_of_over500cc_occupant,
    -no_of_ped_inCarriagewayCrossing,
    -no_of_500cc_occupant,
    -no_of_veh_2000cc,
    -no_veh_leftHandDrive,
    -no_of_scooter_occupant
    )

# Run the logistic regression model
logit1 <- glm(Accident_Severity~., data = train, family = "binomial")
vif <- sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()
vif
summary(logit1)
# Odds Ratio
oddsRatio <- exp(coef(logit1)) %>% data.frame()
oddsRatio

# Determine which variables to omit based on the odds ratio and p-value
train <- train %>% 
  dplyr::select(
    -X1st_Road_Class.2,
    -Speed_limit.10,
    -Junction_Detail.2,
    -Junction_Control.3,
    -X2nd_Road_Class.1,
    -X2nd_Road_Class.2,
    -Pedestrian_Crossing.Human_Control.2,
    -Pedestrian_Crossing.Physical_Facilities.5,
    -Pedestrian_Crossing.Physical_Facilities.7,
    -Weather_Conditions.3,
    -Weather_Conditions.4,
    -Weather_Conditions.7,
    -Weather_Conditions.9,
    -Road_Surface_Conditions.5,
    -Special_Conditions_at_Site.3,
    -no_of_female_drivers,
    -no_taking_toAndFrom_school,
    -no_with_no_impact,
    -no_taxi,
    -no_of_ped_inCarriagewayStationary,
    -no_of_tram_occupant,
    -no_of_goodsveh7.5t_occupant
  )
    

# Run the logistic regression model
logit1 <- glm(Accident_Severity~., data = train, family = "binomial")
vif <- sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()
vif
summary(logit1)
# Evaluate overall significance
overallSignificance <- lrtest(logit1)
overallSignificance
# Evaluate what % of the intercept only model explains churn
mcFadensR2 <- pR2(logit1)
mcFadensR2
# Odds Ratio
oddsRatio <- exp(coef(logit1)) %>% data.frame()
oddsRatio

# Evaluate model performance on training sample
predictedProbabilityForTrain <- logit1$fitted.values
rocForTrain<- roc(train$Accident_Severity,predictedProbabilityForTrain)
rocForTrain
plot(rocForTrain)

predictedProbabilityForTrain %>% data.frame() %>% ggplot() + 
  geom_histogram(aes(., fill = "Red"))
predictedSeverityForTrain <- ifelse(predictedProbabilityForTrain < 0.45, "0", "1")
confusionMatrix(predictedSeverityForTrain, train$Accident_Severity, positive = "1")

# Cross Validation 
trainingFolds <- createFolds(train$Accident_Severity, k = 10, returnTrain = TRUE)
rocForFolds <- lapply(trainingFolds, function(x) {
  trainFold <- train[x,]
  testFold <- train[-x,]
  logitOnFold <- glm(Accident_Severity~., data = trainFold, family = "binomial")
  predictedProbabilityForTestFold <- predict(logitOnFold, newdata = testFold, type = "response")
  rocForTestFold <- roc(testFold$Accident_Severity, predictedProbabilityForTestFold)
  rocForTestFold
})

rocForFoldsAsDataFrame <- data.frame(c(
  rocForFolds$Fold01$auc[1], 
  rocForFolds$Fold02$auc[1], 
  rocForFolds$Fold03$auc[1], 
  rocForFolds$Fold04$auc[1], 
  rocForFolds$Fold05$auc[1], 
  rocForFolds$Fold06$auc[1], 
  rocForFolds$Fold07$auc[1], 
  rocForFolds$Fold08$auc[1], 
  rocForFolds$Fold09$auc[1], 
  rocForFolds$Fold10$auc[1] 
))

plot(rocForFoldsAsDataFrame[,1], type = 'b')

# Evaluate performance on the test sample
predictedProbabilityForTest <- predict(logit1, newdata = test, type = "response")
rocForTest <- roc(test$Accident_Severity, predictedProbabilityForTest)
rocForTest
plot(rocForTest)

predictedSeverityForTest <- ifelse(predictedProbabilityForTest < 0.45, "0", "1")
confusionMatrix(predictedSeverityForTest, test$Accident_Severity, positive = "1")
