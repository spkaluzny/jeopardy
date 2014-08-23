# U.S. population data from:
# http://www.census.gov/popest/data/state/totals/2012/tables/NST-EST2012-01.csv
# Downloaded feb-21-2014
# State populations are in statePop data frame
pop <- pipe("sed -n -e '/Alabama/,/Wyoming/p' NST-EST2012-01.csv | cut -d'\"' --output-delimiter=\";\" -f1-2 | sed -e 's/\"//' -e 's/^.//' -e 's/,//g'")
stateDF <- read.table(p, sep=";", col.names=c("State", "Population"),
    stringsAsFactors=FALSE)
stateDF[stateDF$State == "District of Columbia", "State"] <- "D.C."
CountPlayersByState <- table(jeopardyData$State)
i51 <- match(stateDF$State, names(CountPlayersByState))
stateDF$NPlayers <- CountPlayersByState[i51]
stateDF$PlayersPerMillion <- with(stateDF, NPlayers/Population * 1e6)
require(ggplot2)
p1 <- ggplot(data=stateDF, aes(x=NPlayers,
    y=ordered(State, levels=State[order(stateDF$NPlayers)])))
p1 + geom_point() + xlab("Number of Players") + ylab("State")
p2 <- ggplot(data=stateDF, aes(x=log2(PlayersPerMillion),
    y=ordered(State, levels=State[order(stateDF$PlayersPerMillion)])))
p2 + geom_point()+ xlab("Number of Players / 1 Million Population") + ylab("State")
