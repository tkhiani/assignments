plot(TukeyHSD(anovaWealth))

bigParties <- candidatesInUP[candidatesInUP$assets_in_lacs>100, ]
leveneTest(bigParties$assets_in_lacs~bigParties$party) 
aov_assets_by_party <- aov(bigParties$assets_in_lacs~bigParties$party)
summary(aov_assets_by_party) 

bigParties <- filter(bigParties, bigParties$party %in% c("BJP", "SP", "INC", "BSP"))
