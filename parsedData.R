#IMPORT AND PARSE DATA
library(dplyr)
library(corrplot)
library(tools)
library(Metrics)
library(glmnet)
library(AppliedPredictiveModeling)
library(boot)
library(car)
library(leaps)
library(tidyr)

rawStats.df <- read.csv("17-18-teamBox.csv", stringsAsFactors = F)

dumVar <- c('teamConf','teamDiv','opptConf','opptDiv', 'gmDate', 'gmTime', 'seasTyp', 'offLNm1', 'offFNm1', 'offLNm2',
            'offFNm2', 'offLNm3', 'offFNm3', 'teamMin', 'teamPTS1',
            'teamPTS7', 'teamPTS6', 'teamPTS5', 'teamPTS4', 'teamPTS3', 'teamPTS2',
            'teamPTS1','teamPTS1', 'opptPTS8', 'opptPTS7', 'opptPTS6', 'opptPTS5', 
            'opptPTS4', 'opptPTS3', 'opptPTS2', 'opptPTS1')

relStats.df <- select(rawStats.df,-all_of(dumVar))

relStats.df$teamAbbr <- as.numeric(as.factor(relStats.df$teamAbbr))
relStats.df$teamLoc <- as.numeric(as.factor(relStats.df$teamLoc))
relStats.df$teamRslt <- as.numeric(as.factor(relStats.df$teamRslt))
relStats.df$opptAbbr <- as.numeric(as.factor(relStats.df$opptAbbr))
relStats.df$opptLoc <- as.numeric(as.factor(relStats.df$opptLoc))
relStats.df$opptRslt <- as.numeric(as.factor(relStats.df$opptRslt))

offVar <- c( 'teamLoc', 'teamDayOff','teamPTS', 'teamAST', 'teamTO', 'teamFGM', 'team2PM', 'team3PM', 'teamFTM',
             'teamORB', 'teamTRB', 'teamTREB.', 'teamTS.', 'teamEFG.', 'teamOREB.','teamTO.',
             'teamOrtg', 'teamAST.TO', 'opptDayOff', 'opptSTL', 'opptBLK', 'opptDRB', 'opptDREB.', 
             'opptSTL.', 'opptBLK.', 'opptDrtg')


offStats <- select(relStats.df, all_of(offVar))
offPlot <- cor(offStats)
corrplot(offPlot, method='number')

# avgStats <- final_df

sample_rows <- sample(dim(relStats.df)[1], size = 0.8*dim(relStats.df)[1], replace = F)

df.train <- offStats[sample_rows,]
df.test <- offStats[-sample_rows,]

inputs <- as.matrix(df.train[,-3])
responses <- as.matrix(df.train[,3])
testInputs <- as.matrix(df.test[,-3])
testResponse <- as.matrix(df.test[,3])


# Regular Linear Model
linModel1 <- lm(teamPTS~., data=df.train)
linModel.train <- predict(linModel1, df.train)
linMSE.train <- mean((linModel.train-responses)^2)
linRMSE.train <- rmse(linModel.train, responses)

linModel.test <- predict(linModel1, df.test)
linMSE.test <- mse(linModel.test, testResponse)
linRMSE.test <- rmse(linModel.test, testResponse)

summary(linModel1)

# Standard Variable Selection
linModel2 <- step(linModel1, direction = "both", trace=0)
linModel2.test <- predict(linModel2, df.test)
lin2MSE.test <- mse(linModel2.test, testResponse)
lin2RMSE.test <- rmse(linModel2.test, testResponse)
summary(linModel2)

# LOOCV
# With all variables
linModel.loocv <- glm(teamPTS~., data = df.test)
linModel.loocv.test <- predict(linModel.loocv, df.test)
linLoocv.error <- cv.glm(df.test, linModel.loocv)
linLoocv.error$delta

summary(linModel.loocv)

linLoocv.MSE <- mse(linModel.loocv.test, testResponse)
linLoocv.RMSE <- rmse(linModel.loocv.test, testResponse)

# K-Fold Cross-Validation
linModel.kcv <- glm(teamPTS~., data = offStats)
linModel.kcv.test <- predict(linModel.kcv, df.test)
lin.kcv.error <- cv.glm(offStats, linModel.kcv,K=5)
lin.kcv.error$delta

summary(linModel.kcv)

linModel.kcv.MSE <- mse(linModel.kcv.test, testResponse)
linModel.kcv.RMSE <- rmse(linModel.kcv.test, testResponse)

# Model made up of 25avg + 5avg + PrevGamesAvg



























