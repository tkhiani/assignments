library(dplyr)

casualties <- read.csv('./largeDataSets/capstone - accident/Casualties0515.csv') 

# Remove casualties where do not have an appropriate accident index
casualties <- casualties %>%
  dplyr::filter(!(X...Accident_Index %in%  c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))

head(casualties)
str(casualties)

# Group casualties by accident and capture all attributes
casualtiesByAccident <- casualties %>% 
  dplyr::group_by(X...Accident_Index) %>%
  dplyr::summarise(
    no_of_drivers = sum(Casualty_Class == 1),
    no_of_passengers = sum(Casualty_Class == 2),
    no_of_pedestrains = sum(Casualty_Class == 3),
    no_of_ped_onPedCrossing = sum(Pedestrian_Location == 1),
    no_of_ped_onZigZagApproach = sum(Pedestrian_Location == 2),
    no_of_ped_onZigZagExit = sum(Pedestrian_Location == 3),
    no_of_ped_witin50mofPreCrossing = sum(Pedestrian_Location == 4),
    no_of_ped_inCarriagewayCrossing = sum(Pedestrian_Location == 5),
    no_of_ped_onFootway = sum(Pedestrian_Location == 6),
    no_of_ped_onRefuge = sum(Pedestrian_Location == 7),
    no_of_ped_inCenterOfCarriageway = sum(Pedestrian_Location == 8),
    no_of_ped_inCarriagewayNotCrossing = sum(Pedestrian_Location == 9),
    no_of_ped_PedLocationUnknown = sum(Pedestrian_Location %in% c(10,-1)),
    no_of_ped_crossFromNearside = sum(Pedestrian_Movement == 1),
    no_of_ped_crossFromNearsideMasked = sum(Pedestrian_Movement == 2),
    no_of_ped_crossFromOffSide = sum(Pedestrian_Movement == 3),
    no_of_ped_crossFromOffSideMasked = sum(Pedestrian_Movement == 4),
    no_of_ped_inCarriagewayStationary = sum(Pedestrian_Movement == 5),
    no_of_ped_inCarriagewayStationaryMasked = sum(Pedestrian_Movement == 6),
    no_of_ped_walkingAlongCarriageWayFacingTraffic = sum(Pedestrian_Movement == 7),
    no_of_ped_walkingAlongCarriageWayAgainstTraffic = sum(Pedestrian_Movement == 8),
    no_of_ped_movementUnknown = sum(Pedestrian_Movement %in% c(9,-1)),
    no_of_cyclist = sum(Casualty_Type == 1),
    no_of_50cc_occupant = sum(Casualty_Type == 2),
    no_of_125cc_occupant = sum(Casualty_Type == 3),
    no_of_500cc_occupant = sum(Casualty_Type == 4),
    no_of_over500cc_occupant = sum(Casualty_Type == 5),
    no_of_taxi_occupant = sum(Casualty_Type == 8),
    no_of_car_occupant = sum(Casualty_Type == 9),
    no_of_minibus_occupant = sum(Casualty_Type == 10),
    no_of_bus_occupant = sum(Casualty_Type == 11),
    no_of_hourseriders = sum(Casualty_Type == 16),
    no_of_agrivehicle_occupant = sum(Casualty_Type == 17),
    no_of_tram_occupant = sum(Casualty_Type == 18),
    no_of_goodsveh3.5t_occupant = sum(Casualty_Type == 19),
    no_of_goodsveh7.5t_occupant = sum(Casualty_Type == 20),
    no_of_goodsvehover7.5t_occupant = sum(Casualty_Type == 21),
    no_of_scooter_occupant = sum(Casualty_Type == 22),
    no_of_emotorcycle_occupant = sum(Casualty_Type == 23),
    no_of_otherveh_occupant = sum(Casualty_Type == 90),
    no_of_unknowncc_occupant = sum(Casualty_Type == 97),
    no_of_goodsvenunknownt_occupant = sum(Casualty_Type == 98),
    no_of_female_casualties = sum(Sex_of_Casualty == 2),
    no_of_male_casualties = sum(Sex_of_Casualty == 1),
    no_of_female_casualties = sum(Sex_of_Casualty == 3 | Sex_of_Casualty == -1),
    no_of_casualties_in_ageband_1 = sum(Age_Band_of_Casualty == 1),
    no_of_casualties_in_ageband_2 = sum(Age_Band_of_Casualty == 2),
    no_of_casualties_in_ageband_3 = sum(Age_Band_of_Casualty == 3),
    no_of_casualties_in_ageband_4 = sum(Age_Band_of_Casualty == 4),
    no_of_casualties_in_ageband_5 = sum(Age_Band_of_Casualty == 5),
    no_of_casualties_in_ageband_6 = sum(Age_Band_of_Casualty == 6),
    no_of_casualties_in_ageband_7 = sum(Age_Band_of_Casualty == 7),
    no_of_casualties_in_ageband_8 = sum(Age_Band_of_Casualty == 8),
    no_of_casualties_in_ageband_9 = sum(Age_Band_of_Casualty == 9),
    no_of_casualties_in_ageband_10 = sum(Age_Band_of_Casualty == 10),
    no_of_casualties_in_ageband_11 = sum(Age_Band_of_Casualty == 11),
    no_of_casualties_in_ageband_NotKnown = sum(Age_Band_of_Casualty == -1),
    no_of_fatal_casualties = sum(Casualty_Severity == 1),
    no_of_serious_casualties = sum(Casualty_Severity == 2),
    no_of_fatalserious_casualties = sum(Casualty_Severity %in% c(1,2)),
    no_of_slight_casualties = sum(Casualty_Severity == 3),
    no_of_casualties_in_homearea = sum(Casualty_Home_Area_Type == 1),
    no_of_casualties_notin_homearea = sum(Casualty_Home_Area_Type == -1),
    no_of_casualties_in_roadmainworker = sum(Pedestrian_Road_Maintenance_Worker ==  1)
  ) %>%
  data.frame()

write.csv(casualtiesByAccident, './largeDataSets/capstone - accident/casualtiesByAccident.csv', row.names = FALSE)
