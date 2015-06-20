# Dates:
range(jeopardyData$Date)
# Winnings:
with(jeopardyData, summary(FinalScore[IsWinner]))
# Final Wager:
with(jeopardyData, summary(abs(FinalScore - SecondRoundScore)))
# Games played:
with(jeopardyData, summary(NumTimesOnShow))
# Games played by winners:
with(jeopardyData, summary(NumTimesOnShow[IsWinner]))
# Number of questions answered:
with(jeopardyData, summary(NumRight + NumWrong))
# Number of questions answered by winners:
with(jeopardyData[IsWinner, ], summary(NumRight + NumWrong))
# Percent correct answer:
with(jeopardyData, summary(NumRight/(NumRight + NumWrong)*100))
# Percent correct answer by winners:
with(jeopardyData[IWinner, ], summary(NumRight/(NumRight + NumWrong)*100))
