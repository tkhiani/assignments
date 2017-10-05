library(dplyr)
library(ggplot2)

vehicles <- read.csv('./largeDataSets/capstone - accident/Vehicles0515.csv') 

# Remove vehicles where do not have an appropriate accident index
vehicles <- vehicles %>%
  dplyr::filter(X...Accident_Index != -1) 

head(vehicles)
str(vehicles)

# Group vehicle age into 0, <=5, <=10, <=15, <=20, >20
vehicles %>% 
  dplyr::filter(Age_of_Vehicle < 30 & Age_of_Vehicle > 0) %>%
  ggplot(aes(Age_of_Vehicle)) + geom_histogram(bins = 100)

# Group vehicles into required buckets
vehicles %>% 
  dplyr::filter(Engine_Capacity_.CC. < 5000 & Engine_Capacity_.CC. > 0) %>%
  ggplot(aes(Engine_Capacity_.CC.)) + geom_histogram(bins = 100)

# Group casualties by accident and capture all attributes
vehiclesByAccident <- vehicles %>% 
  dplyr::group_by(X...Accident_Index) %>%
  dplyr::summarise(
    no_of_veh_in_age5 = sum(Age_of_Vehicle >= 0 & Age_of_Vehicle <= 5),
    no_of_veh_in_age10 = sum(Age_of_Vehicle > 5 & Age_of_Vehicle <= 10),
    no_of_veh_in_age15 = sum(Age_of_Vehicle > 10 & Age_of_Vehicle <= 15),
    no_of_veh_in_age20 = sum(Age_of_Vehicle > 15 & Age_of_Vehicle <= 20),
    no_of_veh_in_age21 = sum(Age_of_Vehicle > 20),
    no_of_veh_in_ageNotKnown = sum(Age_of_Vehicle < 0),
    no_of_veh_ccNotKnown = sum(Engine_Capacity_.CC. <= 0),
    no_of_veh_50cc = sum(Engine_Capacity_.CC. > 0 & Engine_Capacity_.CC. <= 50),
    no_of_veh_125cc = sum(Engine_Capacity_.CC. > 50 & Engine_Capacity_.CC. <= 125),
    no_of_veh_500cc = sum(Engine_Capacity_.CC. > 125 & Engine_Capacity_.CC. <= 500),
    no_of_veh_1000cc = sum(Engine_Capacity_.CC. > 500 & Engine_Capacity_.CC. <= 1000),
    no_of_veh_1500cc = sum(Engine_Capacity_.CC. > 1000 & Engine_Capacity_.CC. <= 1500),
    no_of_veh_2000cc = sum(Engine_Capacity_.CC. > 1500 & Engine_Capacity_.CC. <= 2000),
    no_of_veh_3000cc = sum(Engine_Capacity_.CC. > 2000 & Engine_Capacity_.CC. <= 3000),
    no_of_veh_5000cc = sum(Engine_Capacity_.CC. > 3000 & Engine_Capacity_.CC. <= 5000),
    no_of_veh_5001cc = sum(Engine_Capacity_.CC. > 5000),
    no_of_male_drivers = sum(Sex_of_Driver == 1),
    no_of_female_drivers = sum(Sex_of_Driver == 2), 
    no_of_other_drivers = sum(Sex_of_Driver %in% c(3, -1)),
    no_of_drivers_in_home_area = sum(Driver_Home_Area_Type == 1),
    no_of_drivers_in_ageband_2 = sum(Age_Band_of_Driver == 2),
    no_of_drivers_in_ageband_3 = sum(Age_Band_of_Driver == 3),
    no_of_drivers_in_ageband_4 = sum(Age_Band_of_Driver == 4),
    no_of_drivers_in_ageband_5 = sum(Age_Band_of_Driver == 5),
    no_of_drivers_in_ageband_6 = sum(Age_Band_of_Driver == 6),
    no_of_drivers_in_ageband_7 = sum(Age_Band_of_Driver == 7),
    no_of_driverss_in_ageband_8 = sum(Age_Band_of_Driver == 8),
    no_of_drivers_in_ageband_9 = sum(Age_Band_of_Driver == 9),
    no_of_driverss_in_ageband_10 = sum(Age_Band_of_Driver == 10),
    no_of_drivers_in_ageband_11 = sum(Age_Band_of_Driver == 11),
    no_of_drivers_in_ageband_NotKnown = sum(Age_Band_of_Driver == -1),
    no_travelling_for_work = sum(Journey_Purpose_of_Driver == 1),
    no_travelling_toAndFrom_work = sum(Journey_Purpose_of_Driver == 2),
    no_taking_toAndFrom_school = sum(Journey_Purpose_of_Driver == 3),
    no_travelling_toAndFrom_school = sum(Journey_Purpose_of_Driver == 4),
    no_travelling_other = sum(Journey_Purpose_of_Driver == 5),
    no_travelling_notKnown = sum(Journey_Purpose_of_Driver %in% c(6, 15, -1)),
    no_veh_leftHandDrive = sum(Was_Vehicle_Left_Hand_Drive. == 1),
    no_veh_rightHandDrive = sum(Was_Vehicle_Left_Hand_Drive. == 2),
    no_veh_driveNotKnown = sum(Was_Vehicle_Left_Hand_Drive. == -1),
    no_with_no_impact = sum(X1st_Point_of_Impact == 0),
    no_with_front_impact = sum(X1st_Point_of_Impact == 1),
    no_with_back_impact = sum(X1st_Point_of_Impact == 2),
    no_with_offside_impact = sum(X1st_Point_of_Impact == 3),
    no_with_nearside_impact = sum(X1st_Point_of_Impact == 4),
    no_with_impactNotKnown = sum(X1st_Point_of_Impact == -1),
    no_veh_hitObjectOffCarriageway = sum(Hit_Object_off_Carriageway),
    no_veh_hitLeavingCarriageway = sum(Vehicle_Leaving_Carriageway),
    no_veh_hitInCarriageway = sum(Hit_Object_in_Carriageway),
    no_veh_notNextToJunction = sum(Junction_Location == 0),
    no_veh_approachingJunction = sum(Junction_Location == 1),
    no_veh_clearedJunction = sum(Junction_Location == 2),
    no_veh_leavingRoundAbout = sum(Junction_Location == 3),
    no_veh_entringRoundAbout = sum(Junction_Location == 4),
    no_veh_leavingMainRoad = sum(Junction_Location == 5),
    no_veh_entringMainRoad = sum(Junction_Location == 6),
    no_veh_entringSlipRoad = sum(Junction_Location == 7),
    no_veh_midJunction = sum(Junction_Location == 8),
    no_veh_junctionNotKnown = sum(Junction_Location == -1),
    no_veh_reversing = sum(Vehicle_Manoeuvre == 1),
    no_veh_parked = sum(Vehicle_Manoeuvre == 2),
    no_veh_held_up = sum(Vehicle_Manoeuvre == 3),
    no_veh_slowing = sum(Vehicle_Manoeuvre == 4),
    no_veh_movingOff = sum(Vehicle_Manoeuvre == 5),
    no_veh_uTurn = sum(Vehicle_Manoeuvre == 6),
    no_veh_turingLeft = sum(Vehicle_Manoeuvre == 7),
    no_veh_waitTurningLeft = sum(Vehicle_Manoeuvre == 8),
    no_veh_turingRight = sum(Vehicle_Manoeuvre == 9),
    no_veh_waitTuringRight = sum(Vehicle_Manoeuvre == 10),
    no_veh_changingLaneLeft = sum(Vehicle_Manoeuvre == 11),
    no_veh_changingLaneRight = sum(Vehicle_Manoeuvre == 12),
    no_veh_overtakingMovingVehOffside = sum(Vehicle_Manoeuvre == 13),
    no_veh_overtakingStaticVehOffside = sum(Vehicle_Manoeuvre == 14),
    no_veh_overtakingNearside = sum(Vehicle_Manoeuvre == 15),
    no_veh_goingAheadLeftBend = sum(Vehicle_Manoeuvre == 16),
    no_veh_goingAheadRightBend = sum(Vehicle_Manoeuvre == 17),
    no_veh_goingAheadOther = sum(Vehicle_Manoeuvre == 18),
    no_cycles = sum(Vehicle_Type == 1),
    no_motocyle_under_50cc = sum(Vehicle_Type == 2),
    no_motocyle_under_125cc = sum(Vehicle_Type == 3),
    no_motocyle_under_500cc = sum(Vehicle_Type == 4),
    no_motocyle_over_500cc = sum(Vehicle_Type == 5),
    no_taxi = sum(Vehicle_Type == 8),
    no_car = sum(Vehicle_Type == 9),
    no_minibus = sum(Vehicle_Type == 10),
    no_bus = sum(Vehicle_Type == 11),
    no_riddenHorse = sum(Vehicle_Type == 16),
    no_agrVehicle = sum(Vehicle_Type == 17),
    no_tram = sum(Vehicle_Type == 18),
    no_goodsVehicleUnder3.5t = sum(Vehicle_Type == 19),
    no_goodsVehicleUnder7.5t = sum(Vehicle_Type == 20),
    no_goodsVehicleOver7.5t = sum(Vehicle_Type == 21),
    no_mobilityScooter = sum(Vehicle_Type == 22),
    no_eMotorCycle = sum(Vehicle_Type == 23),
    no_otherVehicle = sum(Vehicle_Type == 90),
    no_motocyle_unknowncc = sum(Vehicle_Type == 97),
    no_goodsVehicleUnknownWeight = sum(Vehicle_Type == 98),
    no_vehicleNotKnown = sum(Vehicle_Type == -1)) %>%
  data.frame()

write.csv(vehiclesByAccident, './largeDataSets/capstone - accident/vehiclesByAccident.csv', row.names = FALSE)
