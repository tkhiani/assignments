library(RCurl)
library(XML)
library(dplyr)
library(readxl)

candidateInformation <- function(urlForCandidates, 
                                 urlForCandidateSeriousCrimes, 
                                 urlForFemaleCandidates,
                                 urlForWinningCandidates) {
  candidates <- readHTMLTable(urlForCandidates, which = 3)
  femaleCandidates <- readHTMLTable(urlForFemaleCandidates, which = 3)
  winningCandidates <- readHTMLTable(urlForWinningCandidates, which = 3)
  candidatesWithSeriousCrimes <- readHTMLTable(urlForCandidateSeriousCrimes, which = 3)
  
  # Remove rows 1,2 to clean the data
  candidates <- candidates[-c(1,2),]
  
  # Convernt assets to a meaningful number
  candidates$`Total Assets`<- as.character(candidates$`Total Assets`)
  for (i in 1:nrow(candidates)){
    if(candidates$`Total Assets`[i]=="Nil"){
      candidates$`Total Assets`[i]=0
    }
    else{
      str <- candidates$`Total Assets`[i]
      strn <- gsub(",","",substr(str,start = 4,stop=regexpr("~", str)[1] - 2))
      candidates$`Total Assets`[i] <- strn
    }
  }
  candidates$`Total Assets`<-as.numeric(candidates$`Total Assets`)
  
  candidates <- candidates %>%
    dplyr::mutate(
      assets = `Total Assets`/10000000,
      constituency = `Constituency `,
      education = `Education `,
      criminalCase = `Criminal Case `,
      party = `Party `
    ) %>%
    dplyr::select(Candidate, constituency, education, assets, criminalCase, party)
  
  candidatesWithSeriousCrimes <- candidatesWithSeriousCrimes[-c(1,2),]
  candidatesWithSeriousCrimes <- candidatesWithSeriousCrimes %>%
    dplyr::mutate(
      seriousCrimes = `Criminal Case `,
      party = `Party `,
      constituency = `Constituency `
    ) %>% 
    dplyr::select(Candidate, seriousCrimes, party, constituency)
  
  candidates <- merge(candidates, candidatesWithSeriousCrimes, by = c("Candidate", "constituency", "party"), all = TRUE)
  
  candidates$criminalCase <- as.numeric(candidates$criminalCase)
  candidates$seriousCrimes <- as.numeric(candidates$seriousCrimes)
  
  candidates <- candidates %>% 
    dplyr::mutate(
      seriousCrimes = if_else(is.na(seriousCrimes), 0, seriousCrimes),
      notSeriousCrimes = criminalCase - seriousCrimes
    )

  femaleCandidates <- femaleCandidates[-c(1,2),]
  femaleCandidates <- femaleCandidates %>%
    dplyr::mutate(
      gender = 'F',
      party = `Party `,
      constituency = `Constituency `
    ) %>% 
    dplyr::select(Candidate, party, constituency, gender)
  
  candidates <- merge(candidates, femaleCandidates, by = c("Candidate", "constituency", "party"), all = TRUE)
  
  winningCandidates <- winningCandidates[-c(1,2),]
  winningCandidates <- winningCandidates %>%
    dplyr::mutate(
      seat_won = 1,
      party = `Party `,
      constituency = `Constituency `
    ) %>% 
    dplyr::select(Candidate, party, constituency, seat_won)
  
  candidates <- merge(candidates, winningCandidates, by = c("Candidate", "constituency", "party"), all = TRUE)
  
  candidates$gender <- as.character(candidates$gender)
  candidates$seat_won <- as.numeric(candidates$seat_won)
  
  candidates <- candidates %>% 
    dplyr::mutate(
      gender = if_else(is.na(gender), 'M', gender),
      seat_won = if_else(is.na(seat_won), 0, seat_won)
    )

    return(candidates)
}

#Extract data for candidates in 2009 & 2014
urlCandidates2014 <-"http://www.myneta.info/ls2014/index.php?action=summary&subAction=candidates_analyzed"
urlCandidates2014WithSeriousCriminalCases <- "http://www.myneta.info/ls2014/index.php?action=summary&subAction=serious_crime"
urlCandidates2014Female <- "http://www.myneta.info/ls2014/index.php?action=summary&subAction=women_candidate"
urlCandidates2014WhoWon <- "http://www.myneta.info/ls2014/index.php?action=summary&subAction=winner_analyzed"

urlCandidates2009 <-"http://www.myneta.info/ls2009/index.php?action=summary&subAction=candidates_analyzed"
urlcandidates2009WithSeriousCriminalCases <- "http://www.myneta.info/ls2009/index.php?action=summary&subAction=serious_crime"
urlCandidates2009Female <- "http://www.myneta.info/ls2009/index.php?action=summary&subAction=women_candidate"
urlCandidates2009WhoWon <- "http://www.myneta.info/ls2009/index.php?action=summary&subAction=winner_analyzed"

candidatesIn2009 <- candidateInformation(urlCandidates2009, 
                                         urlcandidates2009WithSeriousCriminalCases, 
                                         urlCandidates2009Female,
                                         urlCandidates2009WhoWon)
write.csv(candidatesIn2009, "./glAssignmnets/GeneralElections - EndToEnd/candidatesIn2009.csv")

candidatesIn2014 <- candidateInformation(urlCandidates2014,
                                         urlCandidates2014WithSeriousCriminalCases,
                                         urlCandidates2014Female,
                                         urlCandidates2014WhoWon)
write.csv(candidatesIn2014, "./glAssignmnets/GeneralElections - EndToEnd/candidatesIn2014.csv")
