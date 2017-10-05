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

accidents <- read.csv('./largeDataSets/capstone - accident/consolidatedAccidentData.csv')

accidents <- accidents %>%
  dplyr::mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
  dplyr::filter(Date > as.Date("30/12/2013", "%d/%m/%Y"))

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
  dplyr::mutate(Accident_Severity = if_else(Accident_Severity %in% c(1,2), 1, 0))

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
    -`Junction_Detail.-1`,
    -`Junction_Control.-1`,
    -`Pedestrian_Crossing.Human_Control.-1`,
    -`Pedestrian_Crossing.Physical_Facilities.-1`,
    -`Carriageway_Hazards.-1`,
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

# Run the logistic regression model. Should we consider interaction affect?
logit1 <- glm(Accident_Severity~., data = train, family = "binomial")
vif <- sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()
vif
summary(logit1)
# Evaluate overall significance
lrtest(logit1)
# Evaluate what % of the intercept only model explains churn
pR2(logit1)

exp(coef(logit1))

# Determine which variables to omit based on the odds ratio and p-value


# Evaluate model performance on training sample
predictedProbabilityForTrain <- logit1$fitted.values
predictedSeverityForTrain <- ifelse(predictedProbabilityForTrain < 0.45, "0", "1")
confusionMatrix(predictedSeverityForTrain, train$Accident_Severity, positive = "1")
rocForTrain<- roc(train$Accident_Severity,predictedProbabilityForTrain)
rocForTrain
plot(rocForTrain)

# Cross Validation 
# trainingFolds <- createFolds(train, k = 10, returnTrain = TRUE)
# build model on the testFold then validate using the trainFold
# repeat the exercise for the 10 folds and plot the ROC for all the folds
# which model should we use - the best or all are approx. the same?

# Evaluate performance on the test sample
predictedProbabilityForTest <- predict(logit1, newdata = test, type = "response")
predictedSeverityForTest <- ifelse(predictedProbabilityForTest < 0.45, "0", "1")
confusionMatrix(predictedSeverityForTest, test$Accident_Severity, positive = "1")
rocForTest <- roc(test$Accident_Severity, predictedProbabilityForTest)
rocForTest
plot(rocForTest)
