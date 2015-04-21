require(dplyr)
require(gender)
require(ggplot2)
firstName <- with(dfJeop02, gsub("^(.*?)\\s.*", "\\1", Name))
# Estimate gender using the gender from the gender package
# Only do the unique firstNames
uFirstName <- unique(firstName)
# This takes 232.14 secs on boulder:
uGenderList <- gender(uFirstName)
uGender <- sapply(uGenderList, function(x) x$gender)
uPropMale <- sapply(uGenderList, function(x) x$proportion_male)
uPropFemale <- sapply(uGenderList, function(x) x$proportion_female)
# Only consider gender that has prop. greater than propCutoff,
# otherwise, declare it as NA
propCutoff <- 0.75
uGender <- ifelse((!is.na(uGender) & uGender == "male" &
    uPropMale < propCutoff), NA, uGender)
uGender <- ifelse((!is.na(uGender) & uGender == "female" &
    uPropFemale < propCutoff), NA, uGender)
mFirstName <- match(firstName, uFirstName)
dfJeop02$Gender <- uGender[mFirstName]
dfJeop02 %>%
    select(Name, Gender) %>%
    sample_n(30, replace = FALSE)

