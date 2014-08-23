# First method, just computes the winning amount and date:
fwList <- split(jeopardyData$Final_Winnings, jeopardyData$Date)
winningAmount <- sapply(fwList, max)
showDate <- as.Date(names(winningAmount))
plot(showDate, winningAmount)

# Second method,compute Winner.p that is TRUE for the the winner of
# each game and add to jeopardyData.
fwList  <- split(jeopardyData$Final_Winnings, jeopardyData$WebID)
fsRank <- c(sapply(fwList, function(w) {
    r <- rank(w)
    if(all(r == 2)) r <- rep(4,3)
    r
}))
Winner.p <- (fsRank == 3 | fsRank == 2.5 | fsRank == 4)
jeopardyData$Winner.p <- Winner.p
require(ggplot2)
p <- ggplot(data=jeopardyData[jeopardyData$Winner.p,]
    aes(x=Date, y=Final_Winnings))
p + geom_point()

# Tied winners
table(fsRank)[c("2.5", "4")]/c(2,3)
