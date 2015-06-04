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
dfJeop02 %>%
    group_by(Show) %>%
    summarise(ShowGender = trigen(Gender)) %>%
    with(table(ShowGender))
# Gender of contestants
dfJeop02 %>%
    group_by(PlayerID) %>%
    summarise(Gender1 = first(Gender)) %>%
    with(table(Gender1))
dfJeop02 %>%
    group_by(PlayerID) %>%
    summarise(Gender1 = last(Gender)) %>%
    with(table(Gender1))  # Should be same as value using first
# Winnings per game by gender:
dfJeop02 %>%
    filter(Winner.p, !is.na(Gender)) %>%
    ggplot(aes(x=Final_Winnings)) + geom_histogram() + facet_grid(Gender ~ .)
# As a density plot
dfJeop02 %>%
    filter(Winner.p, !is.na(Gender)) %>%
    ggplot(aes(x=Final_Winnings)) + geom_density(alpha=0.5) + aes(fill = Gender)
dfJeop02 %>%
    filter(Winner.p, !is.na(Gender)) %>%
    ggplot(aes(x=Final_Winnings)) + geom_histogram() + facet_grid(Gender ~ .)
# Overall winnings by gender:
dfJeop02 %>%
    filter(Winner.p) %>%
    group_by(PlayerID) %>%
    summarise(NumberWins = n(), Gender= first(Gender),
        Winnings = sum(Final_Winnings)) %>%
    filter(NumberWins < 19) %>%
    ggplot(aes(x=Winnings)) + geom_histogram()
