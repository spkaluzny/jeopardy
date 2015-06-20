nWinners <- sapply(split(jeopardyData$FinalScore, jeopardyData$Date),
    function(x) 4 - length(unique(x)))
indxMultpleWinners <- which(nWinners > 1)
indx2Winners <- which(nWinners == 2)
indx3Winners <- which(nWinners == 3)
plot(as.Date(names(nWinners[indxMultpleWinners])), nWinners[indxMultpleWinners],
    type='h', ylim=c(1,3))
