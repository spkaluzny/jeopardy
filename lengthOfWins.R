# Need to combine Name with other variables to get unique ID
numberOfWins <- tapply(jeopardyData[["num_times_on_show"]],
    paste(jeopardyData[["Name"]], jeopardyData[["Occupation"]],
        jeopardyData[["City"]],sep="."), max) - 1
