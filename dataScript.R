# source("jgetid.R")
# id <- 1:5000
# goodID <- jgetid(id)
# write(goodID, "goodID.txt")
# source("jscrapeDirect.R")
# dfJeop01 <- jscrapeDirect(goodID)
# saveRDS(dfJeop01, "dfJeop01.rds")
dfJeop01 <- readRDS("dfJeop01.rds")
#
# Clean the data:
source("jcleanData.R")
dfJeop02 <- jcleanData(dfJeop01)
#
# Add unique PlayerID and Date:
dfJeop02$PlayerID <- with(dfJeop02,
    paste(Name, Occupation, City, State, sep='.'))
airedIndex <- regexpr(" aired ", dfJeop02$Title)
dfJeop02$Date <- as.Date(
    substr(dfJeop02$Title, airedIndex + 7, nchar(dfJeop02$Title)))
# 
# Doubled values
dfJeop02$MaxValue <- ifelse(dfJeop02$Date <= "2001-11-23",
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
dfJeop02 <- group_by(dfJeop02, Show) %>%
    mutate(Winner.p = isWinner(Final_Winnings))
#
# Number of players in final jeopardy
nFinal <-
function(x) {
    rep(sum(x > 0), 3)
}
dfJeop02 <- group_by(dfJeop02, Show) %>%
    mutate(NumberInFinal = nFinal(Final_Winnings))
#                                                                               
# Number of wins
dfJeop02 <- group_by(dfJeop02, PlayerID) %>%
    mutate(NumberWins = n() - 1)
