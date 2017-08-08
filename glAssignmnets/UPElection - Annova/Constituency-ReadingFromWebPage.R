#below is the R-Code. Indeed it's little lengthy. this could be optimized with another loop. 

install.packages("XML")
library(XML)

ConstituenciesByParties <- NULL

http://eciresults.nic.in/ConstituencywiseS0510.htm?ac=10
# Goa
for (i in 1:40){
  ConstituenciesByPartiesTemp <- NULL
  urls <- paste("http://eciresults.nic.in/ConstituencywiseS05",i,".htm?ac=",i,sep = '')
  ConstituenciesByPartiesTemp <- readHTMLTable(urls,which=9)
  lastRow <- nrow(ConstituenciesByPartiesTemp)
  ConstituenciesByPartiesTemp <- ConstituenciesByPartiesTemp[-c(2,lastRow),]
  ConstCode <- ConstituenciesByPartiesTemp[1,1]
  ConstituenciesByPartiesTemp <- cbind(ConstituenciesByPartiesTemp, ConstCode)
  ConstituenciesByPartiesTemp <- ConstituenciesByPartiesTemp[-c(1,2),]
  # ConstituenciesByPartiesTemp$ConstCode <- substr(ConstituenciesByPartiesTemp$ConstCode, 7, 30)
  ConstituenciesByParties <- rbind(ConstituenciesByParties, ConstituenciesByPartiesTemp)
  
}

# Uttar Pradesh
for (i in 1:431){
  ConstituenciesByPartiesTemp <- NULL
  urls <- paste("http://eciresults.nic.in/ConstituencywiseS24",i,".htm?ac=",i,sep = '')
  ConstituenciesByPartiesTemp <- readHTMLTable(urls,which=9)
  lastRow <- nrow(ConstituenciesByPartiesTemp)
  ConstituenciesByPartiesTemp <- ConstituenciesByPartiesTemp[-c(2,lastRow),]
  ConstCode <- ConstituenciesByPartiesTemp[1,1]
  ConstituenciesByPartiesTemp <- cbind(ConstituenciesByPartiesTemp, ConstCode)
  ConstituenciesByPartiesTemp <- ConstituenciesByPartiesTemp[-c(1,2),]
  # ConstituenciesByPartiesTemp$ConstCode <- substr(ConstituenciesByPartiesTemp$ConstCode, 7, 30)
  ConstituenciesByParties <- rbind(ConstituenciesByParties, ConstituenciesByPartiesTemp)
  
}

# Manipur
for (i in 1:60){
  ConstituenciesByPartiesTemp <- NULL
  urls <- paste("http://eciresults.nic.in/ConstituencywiseS14",i,".htm?ac=",i,sep = '')
  ConstituenciesByPartiesTemp <- readHTMLTable(urls,which=9)
  lastRow <- nrow(ConstituenciesByPartiesTemp)
  ConstituenciesByPartiesTemp <- ConstituenciesByPartiesTemp[-c(2,lastRow),]
  ConstCode <- ConstituenciesByPartiesTemp[1,1]
  ConstituenciesByPartiesTemp <- cbind(ConstituenciesByPartiesTemp, ConstCode)
  ConstituenciesByPartiesTemp <- ConstituenciesByPartiesTemp[-c(1,2),]
  # ConstituenciesByPartiesTemp$ConstCode <- substr(ConstituenciesByPartiesTemp$ConstCode, 7, 30)
  ConstituenciesByParties <- rbind(ConstituenciesByParties, ConstituenciesByPartiesTemp)
  
}


# Uttarkhand
for (i in 1:71){
  ConstituenciesByPartiesTemp <- NULL 
  urls <- paste("http://eciresults.nic.in/ConstituencywiseS28",i,".htm?ac=",i,sep = '')
  ConstituenciesByPartiesTemp <- readHTMLTable(urls,which=9)
  lastRow <- nrow(ConstituenciesByPartiesTemp)
  ConstituenciesByPartiesTemp <- ConstituenciesByPartiesTemp[-c(2,lastRow),]
  ConstCode <- ConstituenciesByPartiesTemp[1,1]
  ConstituenciesByPartiesTemp <- cbind(ConstituenciesByPartiesTemp, ConstCode)
  ConstituenciesByPartiesTemp <- ConstituenciesByPartiesTemp[-c(1,2),]
  # ConstituenciesByPartiesTemp$ConstCode <- substr(ConstituenciesByPartiesTemp$ConstCode, 7, 30)
  ConstituenciesByParties <- rbind(ConstituenciesByParties, ConstituenciesByPartiesTemp)
  
}

# Punjab
for (i in 1:117){
  ConstituenciesByPartiesTemp <- NULL
  urls <- paste("http://eciresults.nic.in/ConstituencywiseS19",i,".htm?ac=",i,sep = '')
  ConstituenciesByPartiesTemp <- readHTMLTable(urls,which=9)
  lastRow <- nrow(ConstituenciesByPartiesTemp)
  ConstituenciesByPartiesTemp <- ConstituenciesByPartiesTemp[-c(2,lastRow),]
  ConstCode <- ConstituenciesByPartiesTemp[1,1]
  ConstituenciesByPartiesTemp <- cbind(ConstituenciesByPartiesTemp, ConstCode)
  ConstituenciesByPartiesTemp <- ConstituenciesByPartiesTemp[-c(1,2),]
  # ConstituenciesByPartiesTemp$ConstCode <- substr(ConstituenciesByPartiesTemp$ConstCode, 7, 30)
  ConstituenciesByParties <- rbind(ConstituenciesByParties, ConstituenciesByPartiesTemp)
  
}

names(ConstituenciesByParties) <- c("Candidate","Party","Votes","Constituency")
ConstituenciesByParties$Votes <- as.numeric(as.character(ConstituenciesByParties$Votes))

write.csv(ConstituenciesByParties, "VotesBYConstituency.csv", row.names = FALSE)