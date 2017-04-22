library("dplyr")
library("car")

#load the file on assets for all the candidates 
votesByConstituency <- read.csv("./data/VotesByConstituency.csv")
head(votesByConstituency)

#Summarize by Constituency and merge with votesByConstituency to dermine the % of votes for each candidate  
totalVotesByConstituency <- ddply(votesByConstituency, c("Constituency"), summarize, total = sum(Votes))
votesByConstituencyWithTotals <- merge(votesByConstituency, totalVotesByConstituency, by = "Constituency")
votesByConstituencyWithTotals$percentageVotes <- (votesByConstituencyWithTotals$Votes/votesByConstituencyWithTotals$total) * 100
head(votesByConstituencyWithTotals)

#Let us focus only in the state of Goa  
votesByConstituencyInGoa <- filter(votesByConstituencyWithTotals, grepl("Goa", votesByConstituencyWithTotals$Constituency))
View(votesByConstituencyInGoa)
# Not normally distributed
shapiro.test(votesByConstituencyInGoa$percentageVotes)

# Variances are different. Violates the assumption but not extremely important if the group sizes are the same
leveneTest(votesByConstituencyInGoa$percentageVotes~votesByConstituencyInGoa$Party)

#Showcases that there is a significant differences 
aov_goa <- aov(votesByConstituencyInGoa$percentageVotes~votesByConstituencyInGoa$Party)
summary(aov_goa)

#Difference is significant between 
goa_differences <- TukeyHSD(aov_goa)
plot(goa_differences)
goa_differences <- data.frame(goa_differences[1])

goa_significant_differences <- subset(goa_differences, votesByConstituencyInGoa.Party.p.adj < 0.05)
goa_significant_differences[1]
