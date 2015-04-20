require(dplyr)
require(gender)
require(ggplot2)
dfJeop03 <- select(dfJeop02, Name, PlayerID) %>%
    group_by(PlayerID) %>%
    summarise(Name1 = first(Name))
# Extract first name:
firstName <- with(dfJeop03, gsub("^(.*?)\\s.*", "\\1", Name1))
# Estimate gender using the gender from the gender package
# Only do the unique firstNames
uFirstName <- unique(firstName)
# This takes 237.73 secs. on boulder:
uGenderList <- gender(uFirstName)
uGender <- sapply(uGenderList, function(x) x$gender)
uPropMale <- sapply(uGenderList, function(x) x$proportion_male)
mFirstName <- match(firstName, uFirstName)
