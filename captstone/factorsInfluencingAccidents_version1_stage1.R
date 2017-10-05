# Here we eliminate variables primarly based on the VIF Score one after another
library(dplyr)
library(dummies)
library(car)
library(caret)
library(pROC)

options(max.print=2000)

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
    -Police_Force,
    -X1st_Road_Number,
    -X2nd_Road_Number,
    -Did_Police_Officer_Attend_Scene_of_Accident)

colnames(accidents)

for(i in c(4:19)) {
  accidents[,i] <- as.factor(accidents[,i]) 
}

str(accidents)

accidents <- accidents %>%
  dplyr::mutate(Accident_Severity = if_else(Accident_Severity %in% c(1,2), 1, 0))

Accident_Severity <- accidents$Accident_Severity
accidents <- dummy.data.frame(accidents[-1], sep = ".")
accidents$Accident_Severity <- as.factor(Accident_Severity)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial", control = list(maxit = 200))
summary(logit1)
alias(logit1)

# Remove variables that have a linear relationship as indicated by alias & their coeffecitents are also NA
accidents <- accidents %>% 
  dplyr::select(
    -Day_of_Week.7,
    -X1st_Road_Class.6,
    -Road_Type.9,
    -Speed_limit.70,
    -Junction_Detail.9,
    -Junction_Control.4,
    -X2nd_Road_Class.6,
    -Pedestrian_Crossing.Human_Control.2,
    -Pedestrian_Crossing.Physical_Facilities.8,
    -Light_Conditions.7,
    -Weather_Conditions.9,
    -Road_Surface_Conditions.5,
    -Special_Conditions_at_Site.7,
    -Carriageway_Hazards.7,
    -Urban_or_Rural_Area.2,
    -Day.5,
    -no_of_veh_in_ageNotKnown,
    -no_of_veh_5001cc,
    -no_of_other_drivers,
    -no_travelling_notKnown,
    -no_veh_driveNotKnown,
    -no_with_impactNotKnown,
    -no_veh_junctionNotKnown,
    -no_vehicleNotKnown,
    -no_of_pedestrains,
    -no_of_ped_PedLocationUnknown,
    -no_of_goodsvenunknownt_occupant,
    -no_of_casualties_in_ageband_NotKnown
    )

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial", control = list(maxit = 200))
alias(logit1)
summary(logit1)

sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove Number of Casualties that has the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-Number_of_Casualties)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove Number of no_veh_notNextToJunction that has the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-no_veh_notNextToJunction)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove Number_of_Vehicles that has the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-Number_of_Vehicles)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove no_veh_goingAheadOther with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-no_veh_goingAheadOther)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove no_of_car_occupant with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-no_of_car_occupant)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove no_car with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-no_car)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove no_with_front_impact with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-no_with_front_impact)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove Junction_Detail.0 with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-Junction_Detail.0)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove Road_Surface_Conditions.1 with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-Road_Surface_Conditions.1)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove no_veh_leftHandDrive with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-no_veh_leftHandDrive)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove `Junction_Control.-1` with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-`Junction_Control.-1`)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove no_of_passengers with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-no_of_passengers)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove Road_Type.6 with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-Road_Type.6)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove no_of_drivers_in_ageband_6 with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-no_of_drivers_in_ageband_6)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove no_of_cyclist with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-no_of_cyclist)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove Light_Conditions.1 with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-Light_Conditions.1)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove no_motocyle_under_125cc with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-no_motocyle_under_125cc)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove `X2nd_Road_Class.-1` with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-`X2nd_Road_Class.-1`)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove no_motocyle_under_50cc with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-no_motocyle_under_50cc)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove no_of_ped_inCarriagewayCrossing with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-no_of_ped_inCarriagewayCrossing)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove Speed_limit.30  with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-Speed_limit.30)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove Weather_Conditions.1  with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-Weather_Conditions.1)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove no_motocyle_over_500cc  with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-no_motocyle_over_500cc)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove no_of_veh_2000cc  with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-no_of_veh_2000cc)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove no_riddenHorse  with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-no_riddenHorse)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove no_motocyle_under_500cc  with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-no_motocyle_under_500cc)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove no_eMotorCycle  with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-no_eMotorCycle)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove no_of_male_drivers  with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-no_of_male_drivers)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove Special_Conditions_at_Site.0   with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-Special_Conditions_at_Site.0)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove no_mobilityScooter   with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-no_mobilityScooter)

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove Pedestrian_Crossing.Physical_Facilities.0    with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-Pedestrian_Crossing.Physical_Facilities.0 )

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

# Remove no_of_unknowncc_occupant    with the highest VIF Score
accidents <- accidents %>% 
  dplyr::select(-no_of_unknowncc_occupant )

logit1 <- glm(Accident_Severity~., data = accidents, family = "binomial")
sort(vif(logit1), decreasing = TRUE) %>% as.data.frame()

summary(logit1)
exp(coef(logit1))

predictedProbability <- logit1$fitted.values
predictedSeverity <- ifelse(predictedProbability<=0.3, "0", "1")
confusionMatrix(predictedSeverity, accidents$Accident_Severity, positive = "1")
roc <- roc(accidents$Accident_Severity,predictedProbability)
roc
plot(roc)
