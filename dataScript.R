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
# Add unique PlayerID and Date:
jeopardyData$PlayerID <- with(jeopardyData,
    paste(Name, Occupation, City, State, sep='.'))
airedIndex <- regexpr(" aired ", jeopardyData$Title)
jeopardyData$Date <- as.Date(
    substr(jeopardyData$Title, airedIndex + 7, nchar(jeopardyData$Title)))
# 
# Doubled values
jeopardyData$MaxValue <- ifelse(jeopardyData$Date <= "2001-11-23",
    "Max1000", "Max2000")
#
# Compute winners - Winner.p
isWinner <-
function (x) 
{
    r <- rank(x)
    if (all(r == 2)) {
        c(4, 4, 4)
    }
    else {
        r
    }
    (r == 3) |(r == 2.5) | (r == 4)
}
library(dplyr)
jeopardyData <- group_by(jeopardyData, Show) %>%
    mutate(Winner.p = isWinner(Final_Winnings))
jeopardyData <- ungroup(jeopardyData)
#
# Number of players in final jeopardy
nFinal <-
function(x) {
    rep(sum(x > 0), 3)
}
jeopardyData <- group_by(jeopardyData, Show) %>%
    mutate(NumberInFinal = nFinal(Final_Winnings))
jeopardyData <- ungroup(jeopardyData)
#                                                                               
# Number of wins
jeopardyData <- group_by(jeopardyData, PlayerID) %>%
    mutate(NumberWins = n() - 1)
jeopardyData <- ungroup(jeopardyData)
