

library(sqldf)
library(car)



df_team_box_score <- read.csv('./2012-18_teamBoxScore.csv/2012-18_teamBoxScoreTest.csv')

df_team_standing <- read.csv('./2012-18_standings.csv/2012-18_standings.csv')

#Keep only the columns we want.
df_team_standing <- df_team_standing[,-c(4,7,10,17,18,19,20,21,25,26,27,28,29,30,34,35,36,37,38,39)]

df_teams <- sqldf("SELECT *, ROW_NUMBER() OVER(ORDER BY teamAbbr) AS team_id FROM (SELECT DISTINCT(teamAbbr) FROM df_team_standing)")


df_team_box_score <- sqldf("SELECT * FROM df_team_box_score WHERE teamLoc = 'Home' AND opptLoc = 'Away'")

df_combined <- sqldf("SELECT a.gmDate, a.teamAbbr, b.rank as teamRank, 
                                CASE 
                                  WHEN a.teamRslt = 'Win' THEN 1
                                  WHEN a.teamRslt = 'Loss' THEN 0
                                  ELSE 0
                                end as teamRslt,  
                             a.teamPTS, b.gameWon as TeamWins, b.gameLost as TeamLoses,
                             CASE 
                                WHEN b.stkType = 'loss' THEN b.stkTot * -1
                                WHEN b.stkType = 'win' THEN b.stkTot 
                                ELSE 0
                             END as teamStreak,
                             b.ptsFor as teamPtsFor,
                             b.ptsAgnst as teamPtsAgnst,
                             b.ptsFor-b.ptsAgnst as teamPtsDiff,
                             a.opptAbbr, c.rank as opptRank,  
                             CASE 
                                  WHEN a.opptRslt = 'Win' THEN 1
                                  WHEN a.opptRslt = 'Loss' THEN 0
                                  ELSE 0
                             end as opptRslt,
                             a.opptPTS, c.gameWon as OpptWins, c.gameLost as OpptLoses,
                             CASE 
                                WHEN c.stkType = 'loss' THEN c.stkTot * -1
                                WHEN c.stkType = 'win' THEN c.stkTot 
                                ELSE 0
                             END as opptStreak,
                             c.ptsFor as opptPtsFor,
                             c.ptsAgnst as opptPtsAgnst,
                             c.ptsFor-c.ptsAgnst as opptPtsDiff
                        FROM  df_team_box_score a INNER JOIN df_team_standing b 
                        ON a.gmDate = b.stDate AND a.teamAbbr = b.teamAbbr 
                        INNER JOIN df_team_standing c ON a.gmDate = c.stDate AND a.opptAbbr = c.teamAbbr")

df_combined <- sqldf("SELECT a.* FROM df_combined a
                            WHERE exists (
                              select 1 from df_combined b 
                              where min(a.teamAbbr,a.opptAbbr) = min(b.teamAbbr,b.opptAbbr)
                              AND max(a.teamAbbr,a.opptAbbr) = max(b.teamAbbr,b.opptAbbr)
                              AND a.gmDate = b.gmDate
                          
                          )")


df_combined<- sqldf("SELECT a.*, b.team_id as team_id, c.team_id as oppt_id FROM df_combined a inner join df_teams b on a.teamAbbr = b.teamAbbr
                    INNER JOIN df_teams c on a.opptAbbr = c.teamAbbr")  

df_combined <- df_combined[, c(1,2,22,3,4,5,6,7,8,9,10,11,12,23,13,14,15,16,17,18,19,20,21)]

#data frame for 2012-2013 season games
df_2012_2013_games <- df_combined[df_combined$gmDate >= '2012-10-30' & df_combined$gmDate <= '2013-04-17' ,]

#data frame for 2013-2014 season 
df_2013_2014_games <- df_combined[df_combined$gmDate >= '2013-10-29' & df_combined$gmDate <= '2014-04-16' ,]

#data frame for 2014-2015 season 
df_2014_2015_games <- df_combined[df_combined$gmDate >= '2014-10-28' & df_combined$gmDate <= '2015-04-15' ,]

#data frame for 2015-2016 season 
df_2015_2016_games <- df_combined[df_combined$gmDate >= '2015-10-27' & df_combined$gmDate <= '2016-04-13' ,]

#data frame for 2016-2017 season 
df_2016_2017_games <- df_combined[df_combined$gmDate >= '2016-10-25' & df_combined$gmDate <= '2017-04-12' ,]

#data frame for 2017-2018 season 
df_2017_2018_games <- df_combined[df_combined$gmDate >= '2017-10-17' & df_combined$gmDate <= '2018-04-11' ,]

df_combined <- df_combined [,-c(1,2,6,13,17)]

df_2012_2013_games <-  df_2012_2013_games [,-c(1,2,6,13,17)]

model1 <- lm(teamRslt ~ . , data = df_combined)

summary(model1)

#data frame for 2012-2013 season 
#df_2012_2013 <- df_team_standing[df_team_standing$stDate >= '2012-10-30' & df_team_standing$stDate <= '2013-04-17' ,]

#data frame for 2013-2014 season 
#df_2013_2014 <- df_team_standing[df_team_standing$stDate >= '2013-10-29' & df_team_standing$stDate <= '2014-04-16' ,]

#data frame for 2014-2015 season 
#df_2014_2015 <- df_team_standing[df_team_standing$stDate >= '2014-10-28' & df_team_standing$stDate <= '2015-04-15' ,]

#data frame for 2015-2016 season 
#df_2015_2016 <- df_team_standing[df_team_standing$stDate >= '2015-10-27' & df_team_standing$stDate <= '2016-04-13' ,]

#data frame for 2016-2017 season 
#df_2016_2017 <- df_team_standing[df_team_standing$stDate >= '2016-10-25' & df_team_standing$stDate <= '2017-04-12' ,]

#data frame for 2017-2018 season 
#df_2017_2018 <- df_team_standing[df_team_standing$stDate >= '2017-10-17' & df_team_standing$stDate <= '2018-04-11' ,]


##Looking at the Raptors 2021_2013 season

df_2012_2013_games_Raptors <- sqldf("SELECT a.*, 1 as home_or_away FROM df_2012_2013_games a WHERE team_id = 28
                                    UNION ALL
                                    SELECT 
                                      oppt_id,
                                      opptRank,
                                      opptRslt,
                                      OpptWins,
                                      OpptLoses,
                                      opptStreak,
                                      opptPtsFor,
                                      opptPtsAgnst,
                                      opptPtsDiff,
                                      team_id,
                                      teamRank,
                                      teamRslt,
                                      TeamWins,
                                      TeamLoses,
                                      teamStreak,
                                      teamPtsFor,
                                      teamPtsAgnst,
                                      teamPtsDiff,
                                      0
                                      FROM df_2012_2013_games a WHERE oppt_id = 28
                                      ")



model_2012 <- lm(teamRslt~.,data = df_2012_2013_games_Raptors)

res <-residuals(model_2012)

qqPlot(res,ylab='Residuals',main='Normal Probability PLot', pch = 19, col = 'blue', xlab = 'Theoretical Quantiles')


summary(model_2012)

