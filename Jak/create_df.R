create_df <- function(N,dataframe){
  team_list <- 1:30
  seasons <- list(list('2012-10-30','2013-04-17'),list('2013-10-29','2014-04-16'),
                  list('2014-10-28','2015-04-15'),list('2015-10-27','2016-04-13'),
                  list('2016-10-25','2017-04-12'),list('2017-10-17','2018-04-11'))
  
  final_df <- dataframe[0,]
  
  for (i in seasons){
    temp_season_df <- dataframe[dataframe$gmDate >= i[[1]] & dataframe$gmDate <= i[[2]] , ] 
    for (j in team_list){
      temp_team_df <- temp_season_df[temp_season_df$teamAbbr == j, ]
      for (row in 1: nrow(temp_team_df)){
        temp_previous_games_df <- temp_team_df[temp_team_df$gmDate < temp_team_df[row,"gmDate"],]
        temp_previous_games_df<-temp_previous_games_df[with(temp_previous_games_df,order(gmDate)), ]
        temp_previous_games_df<-temp_previous_games_df[order(nrow(temp_previous_games_df):1), ]
        temp_previous_games_df<-temp_previous_games_df[1:N,]
        #Oppt Previous Games
        temp_oppt_df <- temp_season_df[temp_season_df$teamAbbr == temp_team_df[row,"opptAbbr"], ]
        temp_oppt_previous_games_df <- temp_oppt_df[temp_oppt_df$gmDate < temp_team_df[row,"gmDate"],]
        temp_oppt_previous_games_df<-temp_oppt_previous_games_df[with(temp_oppt_previous_games_df,order(gmDate)), ]
        temp_oppt_previous_games_df<-temp_oppt_previous_games_df[1:N,]
        if(nrow(temp_previous_games_df) < N & nrow(temp_oppt_previous_games_df) < N){
          temp_final_df = data.frame(temp_team_df[row,1],temp_team_df[row,2],
                                     temp_team_df[row,3],temp_team_df[row,4],
                                     temp_team_df[row,5],temp_team_df[row,6],
                                     rep(NA,40),
                                     temp_team_df[row,46],temp_team_df[row,47],
                                     temp_team_df[row,48],temp_team_df[row,49],
                                     temp_team_df[row,50],
                                     rep(NA,40),
                                     temp_team_df[row,90],temp_team_df[row,91],
                                     temp_team_df[row,92],temp_team_df[row,93])
          
        }
        else if(nrow(temp_previous_games_df) >= N & nrow(temp_oppt_previous_games_df) < N){
          temp_final_df = data.frame(temp_team_df[row,1],temp_team_df[row,2],
                                     temp_team_df[row,3],temp_team_df[row,4],
                                     temp_team_df[row,5],temp_team_df[row,6],mean(temp_previous_games_df[,6]),
                                     mean(temp_previous_games_df[,7]),mean(temp_previous_games_df[,8]),
                                     mean(temp_previous_games_df[,9]),mean(temp_previous_games_df[,10]),
                                     mean(temp_previous_games_df[,11]),mean(temp_previous_games_df[,12]),
                                     mean(temp_previous_games_df[,13]),mean(temp_previous_games_df[,14]),
                                     mean(temp_previous_games_df[,15]),mean(temp_previous_games_df[,16]),
                                     mean(temp_previous_games_df[,17]),mean(temp_previous_games_df[,18]),
                                     mean(temp_previous_games_df[,19]),mean(temp_previous_games_df[,20]),
                                     mean(temp_previous_games_df[,21]),mean(temp_previous_games_df[,22]),
                                     mean(temp_previous_games_df[,23]),mean(temp_previous_games_df[,24]),
                                     mean(temp_previous_games_df[,25]),mean(temp_previous_games_df[,26]),
                                     mean(temp_previous_games_df[,27]),mean(temp_previous_games_df[,28]),
                                     mean(temp_previous_games_df[,29]),mean(temp_previous_games_df[,30]),
                                     mean(temp_previous_games_df[,31]),mean(temp_previous_games_df[,32]),
                                     mean(temp_previous_games_df[,33]),mean(temp_previous_games_df[,34]),
                                     mean(temp_previous_games_df[,35]),mean(temp_previous_games_df[,36]),
                                     mean(temp_previous_games_df[,37]),mean(temp_previous_games_df[,38]),
                                     mean(temp_previous_games_df[,39]),mean(temp_previous_games_df[,40]),
                                     mean(temp_previous_games_df[,41]),mean(temp_previous_games_df[,42]),
                                     mean(temp_previous_games_df[,43]),mean(temp_previous_games_df[,44]),
                                     mean(temp_previous_games_df[,45]),
                                     temp_team_df[row,46],temp_team_df[row,47],
                                     temp_team_df[row,48],temp_team_df[row,49],
                                     temp_team_df[row,50],
                                     rep(NA,40),
                                     temp_team_df[row,90],temp_team_df[row,91],
                                     temp_team_df[row,92],temp_team_df[row,93])
          
        }
        else if(nrow(temp_previous_games_df) < N & nrow(temp_oppt_previous_games_df) >= N){
          temp_final_df = data.frame(temp_team_df[row,1],temp_team_df[row,2],
                                     temp_team_df[row,3],temp_team_df[row,4],
                                     temp_team_df[row,5],temp_team_df[row,6],
                                     rep(NA,40),
                                     temp_team_df[row,46],temp_team_df[row,47],
                                     temp_team_df[row,48],temp_team_df[row,49],
                                     temp_team_df[row,50], mean(temp_oppt_previous_games_df[,6]),
                                     mean(temp_oppt_previous_games_df[,7]),mean(temp_oppt_previous_games_df[,8]),
                                     mean(temp_oppt_previous_games_df[,9]),mean(temp_oppt_previous_games_df[,10]),
                                     mean(temp_oppt_previous_games_df[,11]),mean(temp_oppt_previous_games_df[,12]),
                                     mean(temp_oppt_previous_games_df[,13]),mean(temp_oppt_previous_games_df[,14]),
                                     mean(temp_oppt_previous_games_df[,15]),mean(temp_oppt_previous_games_df[,16]),
                                     mean(temp_oppt_previous_games_df[,17]),mean(temp_oppt_previous_games_df[,18]),
                                     mean(temp_oppt_previous_games_df[,19]),mean(temp_oppt_previous_games_df[,20]),
                                     mean(temp_oppt_previous_games_df[,21]),mean(temp_oppt_previous_games_df[,22]),
                                     mean(temp_oppt_previous_games_df[,23]),mean(temp_oppt_previous_games_df[,24]),
                                     mean(temp_oppt_previous_games_df[,25]),mean(temp_oppt_previous_games_df[,26]),
                                     mean(temp_oppt_previous_games_df[,27]),mean(temp_oppt_previous_games_df[,28]),
                                     mean(temp_oppt_previous_games_df[,29]),mean(temp_oppt_previous_games_df[,30]),
                                     mean(temp_oppt_previous_games_df[,31]),mean(temp_oppt_previous_games_df[,32]),
                                     mean(temp_oppt_previous_games_df[,33]),mean(temp_oppt_previous_games_df[,34]),
                                     mean(temp_oppt_previous_games_df[,35]),mean(temp_oppt_previous_games_df[,36]),
                                     mean(temp_oppt_previous_games_df[,37]),mean(temp_oppt_previous_games_df[,38]),
                                     mean(temp_oppt_previous_games_df[,39]),mean(temp_oppt_previous_games_df[,40]),
                                     mean(temp_oppt_previous_games_df[,41]),mean(temp_oppt_previous_games_df[,42]),
                                     mean(temp_oppt_previous_games_df[,43]),mean(temp_oppt_previous_games_df[,44]),
                                     mean(temp_oppt_previous_games_df[,45]),temp_team_df[row,90],temp_team_df[row,91],
                                     temp_team_df[row,92],temp_team_df[row,93])
        }
        else{
          temp_final_df = data.frame(temp_team_df[row,1],temp_team_df[row,2],
                                     temp_team_df[row,3],temp_team_df[row,4],
                                     temp_team_df[row,5],temp_team_df[row,6],
                                     mean(temp_previous_games_df[,6]),
                                     mean(temp_previous_games_df[,7]),mean(temp_previous_games_df[,8]),
                                     mean(temp_previous_games_df[,9]),mean(temp_previous_games_df[,10]),
                                     mean(temp_previous_games_df[,11]),mean(temp_previous_games_df[,12]),
                                     mean(temp_previous_games_df[,13]),mean(temp_previous_games_df[,14]),
                                     mean(temp_previous_games_df[,15]),mean(temp_previous_games_df[,16]),
                                     mean(temp_previous_games_df[,17]),mean(temp_previous_games_df[,18]),
                                     mean(temp_previous_games_df[,19]),mean(temp_previous_games_df[,20]),
                                     mean(temp_previous_games_df[,21]),mean(temp_previous_games_df[,22]),
                                     mean(temp_previous_games_df[,23]),mean(temp_previous_games_df[,24]),
                                     mean(temp_previous_games_df[,25]),mean(temp_previous_games_df[,26]),
                                     mean(temp_previous_games_df[,27]),mean(temp_previous_games_df[,28]),
                                     mean(temp_previous_games_df[,29]),mean(temp_previous_games_df[,30]),
                                     mean(temp_previous_games_df[,31]),mean(temp_previous_games_df[,32]),
                                     mean(temp_previous_games_df[,33]),mean(temp_previous_games_df[,34]),
                                     mean(temp_previous_games_df[,35]),mean(temp_previous_games_df[,36]),
                                     mean(temp_previous_games_df[,37]),mean(temp_previous_games_df[,38]),
                                     mean(temp_previous_games_df[,39]),mean(temp_previous_games_df[,40]),
                                     mean(temp_previous_games_df[,41]),mean(temp_previous_games_df[,42]),
                                     mean(temp_previous_games_df[,43]),mean(temp_previous_games_df[,44]),
                                     mean(temp_previous_games_df[,45]),
                                     temp_team_df[row,46],temp_team_df[row,47],
                                     temp_team_df[row,48],temp_team_df[row,49],
                                     temp_team_df[row,50],mean(temp_oppt_previous_games_df[,6]),
                                     mean(temp_oppt_previous_games_df[,7]),mean(temp_oppt_previous_games_df[,8]),
                                     mean(temp_oppt_previous_games_df[,9]),mean(temp_oppt_previous_games_df[,10]),
                                     mean(temp_oppt_previous_games_df[,11]),mean(temp_oppt_previous_games_df[,12]),
                                     mean(temp_oppt_previous_games_df[,13]),mean(temp_oppt_previous_games_df[,14]),
                                     mean(temp_oppt_previous_games_df[,15]),mean(temp_oppt_previous_games_df[,16]),
                                     mean(temp_oppt_previous_games_df[,17]),mean(temp_oppt_previous_games_df[,18]),
                                     mean(temp_oppt_previous_games_df[,19]),mean(temp_oppt_previous_games_df[,20]),
                                     mean(temp_oppt_previous_games_df[,21]),mean(temp_oppt_previous_games_df[,22]),
                                     mean(temp_oppt_previous_games_df[,23]),mean(temp_oppt_previous_games_df[,24]),
                                     mean(temp_oppt_previous_games_df[,25]),mean(temp_oppt_previous_games_df[,26]),
                                     mean(temp_oppt_previous_games_df[,27]),mean(temp_oppt_previous_games_df[,28]),
                                     mean(temp_oppt_previous_games_df[,29]),mean(temp_oppt_previous_games_df[,30]),
                                     mean(temp_oppt_previous_games_df[,31]),mean(temp_oppt_previous_games_df[,32]),
                                     mean(temp_oppt_previous_games_df[,33]),mean(temp_oppt_previous_games_df[,34]),
                                     mean(temp_oppt_previous_games_df[,35]),mean(temp_oppt_previous_games_df[,36]),
                                     mean(temp_oppt_previous_games_df[,37]),mean(temp_oppt_previous_games_df[,38]),
                                     mean(temp_oppt_previous_games_df[,39]),mean(temp_oppt_previous_games_df[,40]),
                                     mean(temp_oppt_previous_games_df[,41]),mean(temp_oppt_previous_games_df[,42]),
                                     mean(temp_oppt_previous_games_df[,43]),mean(temp_oppt_previous_games_df[,44]),
                                     mean(temp_oppt_previous_games_df[,45]),temp_team_df[row,90],temp_team_df[row,91],
                                     temp_team_df[row,92],temp_team_df[row,93])
          
        }
        final_df <- rbind(final_df,temp_final_df)
      }
      
    }

  }
  names(final_df)<-c(names(dataframe[1:6]),"teamAveragePts",names(dataframe[7:50]),"opptAveragePts",names(dataframe[51:93]))
  return(final_df)
   
}  
  

  

