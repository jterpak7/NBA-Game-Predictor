library(ISLR)
install.packages('tree')
library(tree)

Team_Stats$teamRslt <- as.numeric(Team_Stats$teamRslt)
boxplot(teamElo~teamRslt, data = Team_Stats)
boxplot(opptElo~teamRslt, data = Team_Stats)
boxplot(teamOrtg~teamRslt, data = Team_Stats)
boxplot(teamFIC~teamRslt, data = Team_Stats)
boxplot(teamEFG.~teamRslt, data = Team_Stats)
boxplot(opptFIC~teamRslt, data = Team_Stats)


Importance_TreePlot <- tree(teamRslt~., data=Importance_Training)
plot(Importance_TreePlot)
text(Importance_TreePlot)

RFE_TreePlot <- tree(teamRslt~., data=RFE_Training)
plot(RFE_TreePlot)
text(RFE_TreePlot)
