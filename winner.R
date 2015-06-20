# First method, just computes the winning amount and date:
fwList <- split(jeopardyData$FinalScore, jeopardyData$Date)
winningAmount <- sapply(fwList, max)
showDate <- as.Date(names(winningAmount))
plot(showDate, winningAmount)

# Second method,compute IsWinner that is TRUE for the the winner of
# each game and add to jeopardyData.
fwList  <- split(jeopardyData$FinalScore, jeopardyData$WebID)
fsRank <- c(sapply(fwList, function(w) {
    r <- rank(w)
    if(all(r == 2)) r <- rep(4,3)
    r
}))
IsWinner <- (fsRank == 3 | fsRank == 2.5 | fsRank == 4)
jeopardyData$IsWinner <- IsWinner
require(ggplot2)
p <- ggplot(data=jeopardyData[jeopardyData$IsWinner, ]
    aes(x=Date, y=FinalScore))
p + geom_point()

# Tied winners
table(fsRank)[c("2.5", "4")]/c(2,3)
