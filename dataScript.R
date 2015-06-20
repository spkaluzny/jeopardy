# source("jgetid.R")
# id <- 1:5000
# goodID <- jgetid(id)
# write(goodID, "goodID.txt")
# source("jscrapeDirect.R")
# jeopardyDataRaw <- jscrapeDirect(goodID)
# saveRDS(jeopardyDataRaw, "jeopardyDataRaw.rds")
jeopardyDataRaw <- readRDS("jeopardyDataRaw.rds")
#
#
# Clean the data:
source("jcleanData.R")
jeopardyData <- jcleanData(jeopardyDataRaw)
#
# Fix names:
indx <- which(names(jeopardyData) == "num_times_on_show")
if(length(indx)) {
    names(jeopardyData)[indx] <- "NumTimesOnShow"
}
indx <- which(names(jeopardyData) == "first_round_winnings")
if(length(indx)) {
    names(jeopardyData)[indx] <- "FirstRoundScore"
}
indx <- which(names(jeopardyData) == "Winnings_2nd_Round")
if(length(indx)) {
    names(jeopardyData)[indx] <- "SecondRoundScore"
}
indx <- which(names(jeopardyData) == "Final_Winnings")
if(length(indx)) {
    names(jeopardyData)[indx] <- "FinalScore"
}
indx <- which(names(jeopardyData) == "n.Right")
if(length(indx)) {
    names(jeopardyData)[indx] <- "NumRight"
}
indx <- which(names(jeopardyData) == "n.Wrong")
if(length(indx)) {
    names(jeopardyData)[indx] <- "NumWrong"
}
indx <- which(names(jeopardyData) == "DD.Wrong")
if(length(indx)) {
    names(jeopardyData)[indx] <- "DDWrong"
}
indx <- which(names(jeopardyData) == "DD.Right")
if(length(indx)) {
    names(jeopardyData)[indx] <- "DDRight"
}

# Add unique PlayerID and Date:
jeopardyData$PlayerId <- with(jeopardyData,
    paste(Name, Occupation, City, State, sep='.'))
airedIndex <- regexpr(" aired ", jeopardyData$Title)
jeopardyData$Date <- as.Date(
    substr(jeopardyData$Title, airedIndex + 7, nchar(jeopardyData$Title)))
# 
# Doubled values
jeopardyData$GameMaxValue <- ifelse(jeopardyData$Date <= "2001-11-23",
    "Max1000", "Max2000")
#
# Compute winners - Winner.p
isWinner <-
function (x) 
{
    r <- rank(x)
    if (all(r == 2)) {
        r <- c(4, 4, 4)
    }
    (r == 3) | (r == 2.5) | (r == 4)
}

library(dplyr)
jeopardyData <- group_by(jeopardyData, Show) %>%
    mutate(IsWinner = isWinner(FinalScore))
jeopardyData <- ungroup(jeopardyData)
#
# Number of players in final jeopardy
nFinal <-
function(x) {
    rep(sum(x > 0), 3)
}
jeopardyData <- group_by(jeopardyData, Show) %>%
    mutate(NumberInFinal = nFinal(FinalScore))
jeopardyData <- ungroup(jeopardyData)
#
# Number of wins
jeopardyData <- group_by(jeopardyData, PlayerId) %>%
    mutate(NumberWins = n() - 1)
jeopardyData <- ungroup(jeopardyData)
