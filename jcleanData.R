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
    indx <- which(df$State == "Onio")
    if(length(indx)) {
        df[indx, "State"] <- "Ohio"
    }
    indx <- which(df$State == "Caifornia")
    if(length(indx)) {
        df[indx, "State"] <- "California"
    }
    indx <- which(df$State == "IL")
    if(length(indx)) {
        df[indx, "State"] <- "Illinois"
    }
    indx <- which(df$State == "Kenrucky")
    if(length(indx)) {
        df[indx, "State"] <- "Kentucky"
    }
    indx <- which(df$State == "New Nork")
    if(length(indx)) {
        df[indx, "State"] <- "New York"
    }
    indx <- which(df$State == "British Colombia")
    if(length(indx)) {
        df[indx, "State"] <- "British Columbia"
    }
    indx <- which(df$State == "British Colombia")
    if(length(indx)) {
        df[indx, "State"] <- "British Columbia"
    }
    indx <- which(df$State == "Ontario Canada")
    if(length(indx)) {
        df[indx, "State"] <- "Ontario, Canada"
    }
    # State is just Canada, add Province:
    indx <- which(df$City == "Toronto" & df$State == "Canada")
    if(length(indx)) {
                df[indx, "State"] <- "Ontario, Canada"
    }
    indx <- which(df$City == "Sault Ste. Marie" & df$State == "Canada")
    if(length(indx)) {
                df[indx, "State"] <- "Ontario, Canada"
    }
    indx <- which(df$City == "Ottawa" & df$State == "Canada")
    if(length(indx)) {
                df[indx, "State"] <- "Ontario, Canada"
    }
    indx <- which(df$City == "Guelph" & df$State == "Canada")
    if(length(indx)) {
                df[indx, "State"] <- "Ontario, Canada"
    }
    indx <- which(df$City == "Winnipeg" & df$State == "Canada")
    if(length(indx)) {
                df[indx, "State"] <- "Manitoba, Canada"
    }
    indx <- which(df$City == "Montreal" & df$State == "Canada")
    if(length(indx)) {
                df[indx, "State"] <- "Quebec, Canada"
    }
    indx <- which(df$City == "Vancouver" & df$State == "Canada")
    if(length(indx)) {
                df[indx, "State"] <- "British Columbia, Canada"
    }
    indx <- which(df$City == "West Vancouver" & df$State == "Canada")
    if(length(indx)) {
                df[indx, "State"] <- "British Columbia, Canada"
    }
    # Add Canada to Provinces:
    for(p in c("Alberta", "British Columbia", "Manitoba",
        "Northwest Territories", "Nova Scotia", "Ontario",
        "Prince Edward Island",
        "Quebec", "Saskatchewan", "Yukon Territory")) {
        indx <- which(df$State == p)
        if(length(indx)) {
            df[indx, "State"] <- paste0(p, ", Canada")
        }
    }
    indx <- which(df$State == "Trinidad and Tobago and now in New York")
    if(length(indx)) {
        df[indx, "State"] <- "New York"
    }
    indx <- which(df$State == "Easton Maryland")
    if(length(indx)) {
        df[indx, "State"] <- "Maryland"
        df[indx, "City"] <- "Easton"
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
    indx <- which(df$City == "Bronx" & df$State == "New York")
    if(length(indx)) {
        df[indx, "City"] <- "New York"
        df[indx, "State"] <- "New York"
    }
    indx <- which(df$City == "Brooklyn" & df$State == "New York")
    if(length(indx)) {
        df[indx, "City"] <- "New York"
        df[indx, "State"] <- "New York"
    }
    indx <- which(df$City == "Manhattan" & df$State == "New York")
    if(length(indx)) {
        df[indx, "City"] <- "New York"
        df[indx, "State"] <- "New York"
    }
    indx <- which(df$City == "Queens" & df$State == "New York")
    if(length(indx)) {
        df[indx, "City"] <- "New York"
        df[indx, "State"] <- "New York"
    }
    indx <- which(df$City == "Staten Island" & df$State == "New York")
    if(length(indx)) {
        df[indx, "City"] <- "New York"
        df[indx, "State"] <- "New York"
    }
    # New York City
    indx <- which(df$State == "New York City" | df$City == "New York City")
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
    indx <- grep('nurse and "Mr. Mom"', df$Occupation)
    if(length(indx)) {
        df$Occupation[indx] <- "Nurse"
    }
    indx <- grep('"Mr. Mom"', df$Occupation)
    if(length(indx)) {
                df$Occupation[indx] <- "Dad"
    }
    indx <- grep('Leslie "Lefty" Scott', df$Name)
    if(length(indx)) {
                df$Name[indx] <- "Leslie Scott"
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
