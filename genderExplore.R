require(dplyr)
require(ggplot2)
# Show gender tryad:
trigen <- function(gen) {
    if(any(is.na(gen))) {
        "na"
    } else {
        paste0(sort(gen), collapse = "")
    }
}
jeopardyData %>%
    group_by(Show) %>%
    summarise(ShowGender = trigen(Gender)) %>%
    with(table(ShowGender))
# Gender of contestants
jeopardyData %>%
    group_by(PlayerId) %>%
    summarise(Gender1 = first(Gender)) %>%
    with(table(Gender1))
jeopardyData %>%
    group_by(PlayerId) %>%
    summarise(Gender1 = last(Gender)) %>%
    with(table(Gender1))  # Should be same as value using first
# Winnings per game by gender:
jeopardyData %>%
    filter(IsWinner, !is.na(Gender)) %>%
    ggplot(aes(x=FinalScore)) + geom_histogram() + facet_grid(Gender ~ .)
# As a density plot
jeopardyData %>%
    filter(IsWinner, !is.na(Gender)) %>%
    ggplot(aes(x=FinalScore)) + geom_density(alpha=0.5) + aes(fill = Gender)
jeopardyData %>%
    filter(IsWinner, !is.na(Gender)) %>%
    ggplot(aes(x=FinalScore)) + geom_histogram() + facet_grid(Gender ~ .)
# Overall winnings by gender:
jeopardyData %>%
    filter(IsWinner) %>%
    group_by(PlayerId) %>%
    summarise(NumberWins = n(), Gender= first(Gender),
        Winnings = sum(FinalScore)) %>%
    filter(NumberWins < 19) %>%
    ggplot(aes(x=Winnings)) + geom_histogram()
