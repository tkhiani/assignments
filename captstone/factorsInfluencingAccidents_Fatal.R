# We eliminate variable that 
  # are not needed
  # have high mlticolearnity (that have a high VIF score)
  # not significant and have a odds ratio less than 1
# We then evaluate the model performance with ROC - cross validation and test-train 

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
  dplyr::mutate(Accident_Severity = if_else(Accident_Severity %in% c(2,3), 0, 1))

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

fatalAccidentsInTrain <- dplyr::filter(train, Accident_Severity == "1")
train <- rbind(train, fatalAccidentsInTrain)
train <- rbind(train, fatalAccidentsInTrain)
train <- rbind(train, fatalAccidentsInTrain)

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
    -Light_Conditions.7,
    -Weather_Conditions.9,
    -Urban_or_Rural_Area.2,
    -Day.5,
    -no_of_other_drivers,
    -no_of_pedestrains
    )

# Remove variables that have a VIF score greater than 5
train <- train %>% 
  dplyr::select(
    -Junction_Detail.0,
    -no_veh_notNextToJunction,
    -no_veh_goingAheadOther,
    -Pedestrian_Crossing.Physical_Facilities.0,
    -Number_of_Vehicles,
    -no_of_car_occupant,
    -Junction_Control.4,
    -Special_Conditions_at_Site.0,
    -Road_Surface_Conditions.1,
    -no_with_front_impact,
    -Carriageway_Hazards.0,
    -Number_of_Casualties,
    -no_car,
    -Junction_Detail.3,
    -Road_Type.6,
    -no_of_passengers,
    -no_of_cyclist,
    -Light_Conditions.1,
    -no_motocyle_under_125cc,
    -Pedestrian_Crossing.Human_Control.0,
    -no_motocyle_under_50cc,
    -no_of_male_drivers,
    -Speed_limit.30,
    -Weather_Conditions.1,
    -no_motocyle_over_500cc,
    -no_riddenHorse,
    -no_motocyle_under_500cc,
    -no_of_ped_inCarriagewayCrossing,
    -no_eMotorCycle,
    -no_of_veh_2000cc,
    -no_mobilityScooter,
    -no_veh_leftHandDrive,
    -X2nd_Road_Class.6
  )

# Run the logistic regression model
logit1 <- glm(Accident_Severity~., data = train, family = "binomial")
vif <- sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()
vif
summary(logit1)

oddsRatio <- exp(coef(logit1)) %>% data.frame()
oddsRatio

# Determine which variables to omit based on the odds ratio and p-value
train <- train %>% 
  dplyr::select(
    -Day_of_Week.1,
    -Day_of_Week.2,
    -Day_of_Week.3,
    -Day_of_Week.6,
    -Road_Type.1,
    -Road_Type.2,
    -Road_Type.7,
    -Speed_limit.10,
    -Junction_Detail.2,
    -Junction_Detail.7,
    -Junction_Detail.8,
    -Junction_Control.0,
    -Junction_Control.2,
    -Junction_Control.3,
    -X2nd_Road_Class.3,
    -X2nd_Road_Class.4,
    -Pedestrian_Crossing.Physical_Facilities.1,
    -Pedestrian_Crossing.Physical_Facilities.4,
    -Pedestrian_Crossing.Physical_Facilities.5,
    -Weather_Conditions.3,
    -Weather_Conditions.5,
    -Weather_Conditions.6,
    -Weather_Conditions.7,
    -Road_Surface_Conditions.3,
    -Road_Surface_Conditions.5,
    -Special_Conditions_at_Site.1,
    -Special_Conditions_at_Site.3,
    -Special_Conditions_at_Site.4,
    -Special_Conditions_at_Site.5,
    -Carriageway_Hazards.1,
    -no_of_veh_1500cc,
    -no_of_drivers_in_home_area,
    -no_of_drivers_in_ageband_11,
    -no_taking_toAndFrom_school,
    -no_travelling_toAndFrom_school,
    -no_veh_leavingRoundAbout,
    -no_veh_entringRoundAbout,
    -no_veh_entringSlipRoad,
    -no_veh_reversing,
    -no_veh_waitTurningLeft,
    -no_veh_overtakingMovingVehOffside,
    -no_veh_overtakingNearside,
    -no_veh_goingAheadRightBend,
    -no_tram,
    -no_of_ped_inCarriagewayStationary,
    -no_of_ped_inCarriagewayStationaryMasked,
    -no_of_ped_walkingAlongCarriageWayAgainstTraffic,
    -no_of_50cc_occupant,
    -no_of_taxi_occupant,
    -no_of_agrivehicle_occupant,
    -no_of_goodsveh3.5t_occupant,
    -no_of_goodsveh7.5t_occupant,
    -no_of_emotorcycle_occupant,
    -no_of_female_casualties,
    -no_of_casualties_notin_homearea,
    -no_of_casualties_in_roadmainworker
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
train$predictedProbabilities <- predictedProbabilityForTrain
rocForTrain<- roc(train$Accident_Severity,predictedProbabilityForTrain)
rocForTrain
plot(rocForTrain)

predictedProbabilityForTrain %>% data.frame() %>% ggplot() + 
  geom_histogram(aes(., fill = "Red"))

predictedSeverityForTrain <- ifelse(predictedProbabilityForTrain < 0.2, "0", "1")
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
test$predictedProbabilities <- predictedProbabilityForTest
predictedSeverityForTest <- ifelse(predictedProbabilityForTest < 0.2, "0", "1")
confusionMatrix(predictedSeverityForTest, test$Accident_Severity, positive = "1")
rocForTest <- roc(test$Accident_Severity, predictedProbabilityForTest)
rocForTest
plot(rocForTest)