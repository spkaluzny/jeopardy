require(lubridate)
jeopardyData$playerID <-  paste(jeopardyData[["Name"]],
    jeopardyData[["Occupation"]], jeopardyData[["City"]],
    jeopardyData[["State"]], sep=".")
# Add year info ID:
# jeopardyData$playerID <-  paste(jeopardyData[["Name"]],
#     jeopardyData[["Occupation"]], jeopardyData[["City"]],
#     jeopardyData[["State"]], year(jeopardyData[["Date"]]), sep=".")
with(jeopardyData[jeopardyData$num_times_on_show == 1,],
    max(table(playerID)))
nrow(jeopardyData[jeopardyData$num_times_on_show == 1,])

t <- with(jeopardyData[jeopardyData$num_times_on_show == 1,], table(playerID))
i <- which(t == 2)
i1 <- which(names(t[i])[1] == jeopardyData$playerID)
i2 <- which(names(t[i])[2] == jeopardyData$playerID)
jeopardyData[i1, c(3,7,10,11,16,17)]
jeopardyData[i2, c(3,7,10,11,16,17)]

iak <- which(jeopardyData$Name == "Arianna Kelly")
jeopardyData[iak, c(3,7,10,11,16,17)]

diffNumTimesOnShow <- tapply(jeopardyData$num_times_on_show,
    jeopardyData$playerID, function(x) {
        d <- diff(sort(x))
        if(length(d) && any(d > 1)) {
                d
        } else {
            -1
        }
     })
