library(conjoint)
clickThroughRate <- read.csv("./conjoint/clickThroughRate.csv")
clickThroughRate

y <- clickThroughRate$Click.through.rate
x <- clickThroughRate[,-6]
z <- c("Two","One","Small","Medium","Large","Animated","Static","Long","Short","Text Link","Button")

cModel <- caModel(y,x)
cModel

caUtilities(y,x,z)
caPartUtilities(y,x,z)
Conjoint(y,x,z)