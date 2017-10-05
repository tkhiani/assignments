library(dplyr)

accidents <- read.csv('./largeDataSets/capstone - accident/Accidents0515.csv') 
casualtiesByAccident <- read.csv('./largeDataSets/capstone - accident/casualtiesByAccident.csv') 
vehiclesByAccident <- read.csv('./largeDataSets/capstone - accident/vehiclesByAccident.csv') 

accidents <- accidents %>% 
  dplyr::mutate(
  Date = as.Date(Date, "%d/%m/%Y"), 
  Time_In_Hrs = as.numeric(substring(accidents$Time, 1,2)) + 
    as.numeric(substring(accidents$Time, 4,6))/60,
  Time_In_Hrs = if_else(is.na(Time_In_Hrs), mean(Time_In_Hrs, na.rm = TRUE), Time_In_Hrs)
)

# Convert time into 
# 1 - early morning (4 - 7)
# 2 - morning (8 to 12)
# 3 - afternoon (12 to 4)
# 4 - evening (4 to 8)
# 5 - night (8 to 4)
accidents <- accidents %>%
  dplyr::mutate(
    Day = if_else(Time_In_Hrs >= 4 & Time_In_Hrs < 7, 1, 
                  if_else(Time_In_Hrs >= 7 & Time_In_Hrs < 12, 2, 
                          if_else(Time_In_Hrs >= 12 & Time_In_Hrs < 16, 3,
                                  if_else(Time_In_Hrs >= 16 & Time_In_Hrs < 20, 4, 5)))))


all <- inner_join(accidents, vehiclesByAccident, by = 'X...Accident_Index')
rm(accidents, vehiclesByAccident)
all <- inner_join(all, casualtiesByAccident, by = 'X...Accident_Index')
rm(casualtiesByAccident)

write.csv(all, './largeDataSets/capstone - accident/consolidatedAccidentData.csv', row.names = FALSE)
rm(all)