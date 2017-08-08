library("car")
library("dplyr")

#load the file on assets for all the candidates in the assembly elections in UP 2017
candidatesInUP <- read.csv("./glAssignmnets/UPElection - Annova/UP Candidates.csv")
candidatesInUP$assets_in_lacs <- candidatesInUP$total_assets/100000
View(candidatesInUP)

#Determine if there is significant difference in the asset sizes by party
assets_by_party_aov <- aov(candidatesInUP$assets_in_lacs ~ candidatesInUP$party)
#Showcases that there is significant differences across parties
summary(assets_by_party_aov) 

#Determine if there is significant difference in the asset sizes by party for candidates with more than a crore in assets
candidatesWithLargeAssets <- candidatesInUP[candidatesInUP$assets_in_lacs>100, ]
View(candidatesWithLargeAssets)

assets_by_party_core_plus_aov <- aov(candidatesWithLargeAssets$assets_in_lacs ~ candidatesWithLargeAssets$party)

#Showcases that there is no significant differences across parties for candidates with a crore in assets
summary(assets_by_party_core_plus_aov) 

# Determine if there is significant difference across top 4 parties with asset size more than a crore
candidateInTop4Parties <- subset(candidatesWithLargeAssets, party %in% c("BJP", "SP", "INC", "BSP"))
View(candidateInTop4Parties)

# Not normally distributed
shapiro.test(candidateInTop4Parties$assets_in_lacs) 

# Variances are different. Violates the assumption but not extremely important if the group sizes are the same
leveneTest(candidateInTop4Parties$assets_in_lacs ~ candidateInTop4Parties$party) 
assets_by_top4party_core_plus_aov <- aov(candidateInTop4Parties$assets_in_lacs ~ candidateInTop4Parties$party)

#Showcases that there is a significant differences across parties for candidates with a crore in assets
summary(assets_by_top4party_core_plus_aov) 

#Difference is large between SP & INC
TukeyHSD(assets_by_top4party_core_plus_aov) 
plot(TukeyHSD(assets_by_top4party_core_plus_aov))