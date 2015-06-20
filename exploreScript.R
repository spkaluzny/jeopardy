require(dplyr)
require(ggplot2)
summary(jeopardyData[jeopardyData$IsWinner, "FinalScore"])
group_by(jeopardyData, GameMaxValue) %>%
    summarize(MedianWinnings=median(FinalScore), 
       AveWinnings=mean(FinalScore))
filter(jeopardyData, IsWinner) %>% 
    ggplot(aes(x=FinalScore)) +
    geom_histogram()
filter(jeopardyData, IsWinner) %>% 
    ggplot(aes(x=FinalScore)) +
    geom_histogram() +
    facet_grid(GameMaxValue ~ .)
#
# Number of players in final jeopardy
nFinal<-
function(x) {
   rep(sum(x > 0), 3)
}
df <- group_by(jeopardyData, Show) %>%
    mutate(NumberInFinal = nFinal(FinalScore))
#
# State population data
URL <-
"http://www.census.gov/popest/data/state/totals/2012/tables/NST-EST2012-01.csv"
download.file(URL, dest="NST-EST2012-01.csv")
pop <- pipe("sed -n -e '/Alabama/,/Wyoming/p' NST-EST2012-01.csv | cut -d'\"' --output-delimiter=\";\" -f1-2 | sed -e 's/\"//' -e 's/^.//' -e 's/,//g'")
stateDF <- read.table(pop, sep=";", col.names=c("State", "Population"),         
    stringsAsFactors=FALSE)
stateDF[stateDF$State == "District of Columbia", "State"] <- "D.C."
jeopardyData %>% filter(State %in% c(state.name, "D.C.")) %>%
    group_by(State) %>%
    summarise(N=n()) %>%
    ggplot(aes(x=State, y=N)) +
        geom_bar(stat="identity") +
        coord_flip()
jeopardyData %>% filter(State %in% c(state.name, "D.C.")) %>%
    ggplot(aes(x=reorder(State, State, length))) +
        geom_bar(width = 0.6) +
        coord_flip()
jeopardyData %>% filter(State %in% c(state.name, "D.C.")) %>%
    group_by(State) %>%
    summarise(N=n()) %>%
    full_join(stateDF, by="State") %>%
    mutate(PlayersPerMillion = N / Population * 1e6) %>%
    ggplot(aes(x=reorder(State,PlayersPerMillion,mean),
        y=log2(PlayersPerMillion))) +
        geom_bar(stat="identity") +
        coord_flip() +
        ylab("Number of Players / 1 Million Population (log2)") +
        xlab("State")
#
# Number of wins
group_by(jeopardyData, PlayerId) %>%
    mutate(NumberWins = n() - 1) %>%
    summarise(Wins = first(NumberWins)) %>%
    filter(Wins > 1 & Wins < 19) %>%
    ggplot(aes(x=Wins)) +
        geom_bar(binwidth=0.5) +
        coord_flip()
#
# Dollar winnings per game
filter(jeopardyData, IsWinner) %>%
    filter(FinalScore < 60000) %>%
    ggplot(aes(x=FinalScore)) +
        geom_histogram()
#
# Total winnings per player:
group_by(jeopardyData, PlayerId) %>%
    mutate(NumberWins = n() - 1) %>%
    summarise(Wins = first(NumberWins), Dollars = sum(FinalScore)) %>%
    filter(Wins > 0) %>%
    filter( Wins < 19) %>%
    ggplot(aes(x=Wins, y=Dollars)) +
        geom_jitter(position = position_jitter(width = .3))
# boxplot:    
group_by(jeopardyData, PlayerId) %>%
    mutate(NumberWins = n() - 1) %>%
    summarise(Wins = first(NumberWins), Dollars = sum(FinalScore)) %>%
    filter(Wins > 0) %>%
    filter(Wins < 19) %>%
    ggplot(aes(x=Wins, y=Dollars)) +
        geom_boxplot(aes(group = Wins))
