# DK Historical and Projected Data imported from rotoguru1.com
#

wk1to2actual <- X2017DKWk1to2ActualStats
wk1to3actual <- X2017DKWk1to3ActualStats

#Calculate difference between Points/game of most recent week compared to season up to that week
wk1to3actual$diffScoring <- wk1to3actual$`Pts/Game`- wk1to3actual$`Pts/G(alt)`

#Order by difference to visually inspect
diffOrder <- wk1to3actual[order(wk1to3actual$diffScoring,decreasing = FALSE),]



#plot difference vs. salary change
plot(wk1to3actual$diffScoring, wk1to3actual$`Salary Change`)
abline(lm(wk1to3actual$diffScoring~wk1to3actual$`Salary Change`), col="red") # regression line (y~x)

#no relation seen from salary change to scoring differential, 
#but maybe because most players are clustered around 0 salary change and 0 scoring change
top100diffScore <- diffOrder[1:100,]

plot(top100diffScore$diffScoring,top100diffScore$`Salary Change`)
abline(lm(top100diffScore$diffScoring~top100diffScore$`Salary Change`))
lines(loess(top100diffScore$diffScoring~top100diffScore$`Salary Change`), col="blue")



#change Opponent to factor
top100diffScore$Opponent <- as.factor(top100diffScore$Opponent)
str(top100diffScore)

top100diffScore$`Home/Away` <- as.factor(top100diffScore$`Home/Away`)


pairs(~diffScoring+Opponent+"Home/Away",data=top100diffScore, 
      main="Simple Scatterplot Matrix")

#
library(lattice)

super.sym <- trellis.par.get("superpose.symbol")

splom(top100diffScore[c(7,8,9)], data=top100diffScore, groups='Home/Away',
      panel=panel.superpose, 
      key=list(title="Home vs Away",
               columns=2,
               points=list(pch=super.sym$pch[1:2],
                           col=super.sym$col[1:2]),
               text=list(c("Home","Away"))))
