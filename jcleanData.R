"jcleanData"<-
function(df, dropPilot=TRUE, drop504=TRUE) {
    require("stringr", quietly=TRUE, warn.conflicts=FALSE, character.only=TRUE)
    # Nellis Air Force Base:
    indx <- which(df$City == "Nellis Air Force Base" &
        df$State == "Las Vegas")
    if(length(indx)) {
        df[indx, "City"] <- "Las Vegas"
        df[indx, "State"] <- "Nevada"
    }
    # State misspelled:
    indx <- which(df$State == "Vriginia")
    if(length(indx)) {
        df[indx, "State"] <- "Virginia"
    }
    indx <- which(df$State == "Nevade")
    if(length(indx)) {
        df[indx, "State"] <- "Nevada"
    }
    # State guess
    indx <- which(df$State == "Hawaii[?]")
    if(length(indx)) {
        df[indx, "State"] <- "Hawaii"
    }
    # Yellowstone National Park
    indx <- which(df$State == "Wyoming and Montana")
    if(length(indx)) {
        df[indx, "State"] <- "Wyoming"
    }
    # New York Boroughs
    indx <- which(df$State == "The Bronx")
    if(length(indx)) {
        df[indx, "City"] <- "New York"
        df[indx, "State"] <- "New York"
    }
    # New York City
    indx <- which(df$State == "New York City")
    if(length(indx)) {
        df[indx, "City"] <- "New York"
        df[indx, "State"] <- "New York"
    }
    # West Hartford, no state:
    indx <- which(df$State == "West Hartford")
    if(length(indx)) {
        df[indx, "City"] <- "West Hartford"
        df[indx, "State"] <- "Connecticut"
    }
    # now stationed at ... state
    indx <- grep("now stationed ", df$State)
    if(length(indx)) {
        for(i in indx) {
            stateFirstCommaIndex <- regexpr(',', df$State[i])[1]
            df$State[i] <- substr(df$State[i], 1, stateFirstCommaIndex - 1)
        }
    }
    # now living in ...
    indx <- grep("now living", df$State)
    if(length(indx)) {
        for(i in indx) {
            nowIndex <- regexpr('now living in ', df$State[i])
            newCityState <- substr(df$State[i], nowIndex[1] +
                attr(nowIndex, "match.length"), nchar(df$State[i]))
            cityStateCommaIndex <- gregexpr(',', newCityState)
            if(cityStateCommaIndex[[1]][1] == -1) {
                df$City[i] <- NA
                df$State[i] <- newCityState
            } else {
                df$City[i] <- str_trim(substr(newCityState, 1,
                    cityStateCommaIndex[[1]][1]-1))
                df$State[i] <- str_trim(substr(newCityState,
                    cityStateCommaIndex[[1]][1]+1, nchar(df$State[i])))
            }
        }
    }
    if(dropPilot) {
        indx <- grep("pilot", df$Title)
        if(length(indx)) {
            df <- df[-indx, ]
        }
    }
    if(drop504) {
        indx <- which(df$WebID == 504)
        if(length(indx)) {
            df <- df[-indx, ]
        }
    }
    df
}
