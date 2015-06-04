require(dplyr)
require(gender)
firstName <- with(jeopardyData, gsub("^(.*?)\\s.*", "\\1", Name))
# Estimate gender using the gender from the gender package
# Only do the unique firstNames
uFirstName <- unique(firstName)
# This takes 232.14 secs on boulder:
uGenderList <- gender(uFirstName)
uGender <- sapply(uGenderList, function(x) x$gender)
# Code "male" -> "M", "female" -> "F"
uGender <- ifelse(uGender == "male", "M", uGender)
uGender <- ifelse(uGender == "female", "F", uGender)
uPropMale <- sapply(uGenderList, function(x) x$proportion_male)
uPropFemale <- sapply(uGenderList, function(x) x$proportion_female)
# Only consider gender that has prop. greater than propCutoff,
# otherwise, declare it as NA
propCutoff <- 0.75
uGender <- ifelse((!is.na(uGender) & uGender == "M" &
    uPropMale < propCutoff), NA, uGender)
uGender <- ifelse((!is.na(uGender) & uGender == "F" &
    uPropFemale < propCutoff), NA, uGender)
mFirstName <- match(firstName, uFirstName)
jeopardyData$Gender <- uGender[mFirstName]
#
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
    group_by(PlayerID) %>%
    summarise(Gender1 = first(Gender)) %>%
    with(table(Gender1))
jeopardyData %>%
    group_by(PlayerID) %>%
    summarise(Gender1 = last(Gender)) %>%
    with(table(Gender1))  # Should be same as value using first


