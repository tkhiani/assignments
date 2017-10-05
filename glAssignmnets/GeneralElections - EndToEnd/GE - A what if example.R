library(readxl)
library(plyr)
library(dplyr)
library(ggplot2)
library(randomForest)

# 2009 Election Data

telangana <- c(1, 14, 10, 9, 3, 17, 16, 11, 7, 6, 12, 13, 4, 2, 8, 15, 5)

electionResults <- data.frame(read_excel(path = "./largeDataSets/generalElections - EndToEnd/GE_2009.xls", sheet = "cand_wise")) %>%
  dplyr::mutate(
    SEAT_WON = if_else(POSITION == 1, 1, 0),
    ST_NAME = if_else(ST_NAME == "Andhra Pradesh" & PC_NO %in% telangana, "Telangana", ST_NAME),
    ST_PC = paste(ST_NAME, PC_NO, sep = "_")) %>%
  dplyr::select(ST_PC, ST_NAME, PARTYABBRE, TOTVOTPOLL, SEAT_WON, POSITION)

electors <- data.frame(read_excel(path = "./largeDataSets/generalElections - EndToEnd/GE_2009.xls", sheet = "electors")) %>%
  dplyr::mutate(
    ST_NAME = if_else(ST_NAME == "Andhra Pradesh" & PC_NO %in% telangana, "Telangana", ST_NAME),
    ST_PC = paste(ST_NAME, PC_NO, sep = "_")) %>%
  dplyr::select(ST_PC, TOT_CONTESTANT, TOT_VOTERS)

electionResults <- dplyr::left_join(x = electionResults, y = electors, by = "ST_PC") 

electionResults <- electionResults %>%
  dplyr::mutate(VOTE_SHARE = TOTVOTPOLL*100/TOT_VOTERS) %>%
  dplyr::select(ST_PC, ST_NAME, PARTYABBRE, SEAT_WON, VOTE_SHARE, TOT_CONTESTANT)

# Predict the 2014 GE
# Due to poor performance of the UPA lead government we see 
# an increase in the vote share of BJP in the north, TRS in Telangana, TDP in Andra Pradesh, TMC/AITC in West Bengal
# consistent performance in the rest of south india as earlier - AIADMK (TN)
# corresponding decrease in INC 

election2014 <- electionResults %>% 
  dplyr::mutate(
    VOTE_SHARE = 
      if_else(ST_NAME == 'Telangana', 
              if_else(PARTYABBRE == 'TRS', VOTE_SHARE + 8, if_else(PARTYABBRE %in% c('TDP', 'INC'), VOTE_SHARE - 4, VOTE_SHARE)), VOTE_SHARE),
    VOTE_SHARE = 
      if_else(ST_NAME == 'West Bengal', 
              if_else(PARTYABBRE == 'AITC', VOTE_SHARE + 8, if_else(PARTYABBRE %in% c('CPM', 'AIFB', 'INC', 'CPI'), VOTE_SHARE - 4, VOTE_SHARE)), VOTE_SHARE),
    VOTE_SHARE = 
      if_else(ST_NAME == 'Andhra Pradesh', 
              if_else(PARTYABBRE == 'TDP', VOTE_SHARE + 8, if_else(PARTYABBRE %in% c('TRS', 'INC'), VOTE_SHARE - 4, VOTE_SHARE)), VOTE_SHARE), 
    VOTE_SHARE = 
      if_else(ST_NAME == 'Uttar Pradesh', 
              if_else(PARTYABBRE == 'BJP', VOTE_SHARE + 9, if_else(PARTYABBRE %in% c('INC', 'SP', 'BSP'), VOTE_SHARE - 3, VOTE_SHARE)), VOTE_SHARE), 
    VOTE_SHARE = 
      if_else(ST_NAME == 'Tamil Nadu', 
              if_else(PARTYABBRE == 'ADMK', VOTE_SHARE + 6, if_else(PARTYABBRE %in% c('INC', 'DMK'), VOTE_SHARE - 3, VOTE_SHARE)), VOTE_SHARE), 
    VOTE_SHARE =
      if_else(!(ST_NAME %in% c('Telangana', 'West Bengal', 'Andhra Pradesh', 'Tamil Nadu', 'Kerala', 'Uttar Pradesh')), 
              if_else(PARTYABBRE == 'INC', VOTE_SHARE - 4, if_else(ST_NAME == 'BJP', VOTE_SHARE + 4, VOTE_SHARE)), VOTE_SHARE)
    ) %>%
  dplyr::select(ST_PC, ST_NAME, PARTYABBRE, VOTE_SHARE, TOT_CONTESTANT)

election2014Prediction <- election2014 %>% 
  dplyr::group_by(ST_PC) %>% 
  dplyr::filter(VOTE_SHARE == max(VOTE_SHARE)) %>% 
  dplyr::mutate(SEAT_WON = 1) %>%
  dplyr::ungroup(ST_PC) %>%
  data.frame()

election2014Prediction %>% 
  dplyr::group_by(PARTYABBRE) %>% 
  dplyr::summarise(noOfSeats = sum(SEAT_WON)) %>%
  dplyr::arrange(desc(noOfSeats)) %>%
  dplyr::top_n(10) %>%
  data.frame()
