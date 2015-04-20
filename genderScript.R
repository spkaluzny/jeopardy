require(dplyr)
require(gender)
require(ggplot2)
dfJeop03 <- select(dfJeop02, Name, PlayerID) %>%
    group_by(PlayerID) %>%
    summarise(Name1 = first(Name))
# Extract first name:
firstName <- with(dfJeop03, gsub("^(.*?)\\s.*", "\\1", Name1))
# Estimate gender using the gender from the gedner package
# 
Gender <- sapply(firstName, function(x) gender(x)$gender)


