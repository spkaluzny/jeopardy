# Dates:
range(jeopardyData$Date)
# Winnings:
with(jeopardyData, summary(Final_Winnings[Winner.p]))
# Final Wager:
with(jeopardyData, summary(abs(Final_Winnings - Winnings_2nd_Round)))
# Games played:
with(jeopardyData, summary(num_times_on_show))
# Games played by winners:
with(jeopardyData, summary(num_times_on_show[Winner.p]))
# Number of questions answered:
with(jeopardyData, summary(n.Right + n.Wrong))
# Number of questions answered by winners:
with(jeopardyData[Winner.p,], summary(n.Right + n.Wrong))
# Percent correct answer:
with(jeopardyData, summary(n.Right/(n.Right + n.Wrong)*100))
# Percent correct answer by winners:
with(jeopardyData[Winner.p,], summary(n.Right/(n.Right + n.Wrong)*100))
