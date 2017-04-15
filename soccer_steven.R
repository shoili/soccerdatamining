setwd('/Users/Stevenstuff/Downloads')
#setwd("/Users/Stevenstuff/soccerdatamining")
soccer <- read.csv('Soccerdata.csv')

# fill in blanks with NA's
soccer[soccer==""] <- NA

# betting data as numeric instead of factor
for(i in rep(20:49)) {
  soccer[,i] <- as.numeric(soccer[,i])
}

# remove "diff" columns: time since last update
soccer <- subset(soccer, select=-c(rank_Update_diff1, rank_Update_diff11, 
                                   Update_diff1, Update_diff11))

# buildUpPlayDribbling_home and buildUpPlayDribbling1_away has 19088 NA's
soccer <- subset(soccer, select=-c(buildUpPlayDribbling_home, buildUpPlayDribbling1_away))

# remove duplicate columns
soccer <- subset(soccer, select=-c(country_id1, league_id, team_api_id, 
                                   team_api_id1, team_api_id1_away_away, 
                                   id3_home, id4, team_fifa_api_id2_home,
                                   team_api_id2_home, team_fifa_api_id3_away,
                                   team_api_id3_away))
# remove useless columns
soccer <- subset(soccer, select=-c(id, date, home_team_goal, 
                                   away_team_goal, Goal_Diff_Home, 
                                   Goal_Diff_away, id1_home_home, team_fifa_api_id_home_home,
                                   team_long_name_home_home, team_short_name_home_home,
                                   id2_away_away,team_fifa_api_id1_away_away,team_long_name1_away_away,
                                   team_short_name1_away_away,Country_Name_home,League_name,id5,date1_home,
                                   id6_away,date2_away))

# factorize columns
names<-c("country_id", "season", "stage", "match_api_id", "home_team_api_id", 
         "away_team_api_id")
soccer[,names] <- lapply(soccer[,names], factor)

# remove rows with NA
soccer_nona <- na.omit(soccer)

write.csv(soccer, 'soccer_clean.csv', row.names=FALSE)
write.csv(soccer_nona, 'soccer_nona.csv', row.names=FALSE)


# multinomial regression: 1 if home wins, 2 if home loses, 3 if tie
soccer$outcome <- ifelse(soccer$home_team_goal-soccer$away_team_goal > 0, 1, 
                         ifelse(soccer$home_team_goal-soccer$away_team_goal<0, 2, 3))
soccer$outcome <- as.factor(soccer$outcome)


library(nnet)
test <- multinom(outcome ~ country_id+season+stage+match_api_id+home_team_api_id+
                   away_team_api_id+Goal_Diff_Home_10.Games+Goal_Diff_away_10.Games+
                   B365H+B365D+B365A+BWH+BWD+BWA+IWH+IWD+IWA+LBH+LBD+LBA+PSH+PSD+PSA+
                   WHH+WHD+WHA+SJH+SJD+SJA+VCH+VCD+VCA+GBH+GBD+GBA+BSH+BSD+BSA+
                   buildUpPlaySpeed_home+buildUpPlaySpeedClass_home+
                   buildUpPlayDribblingClass_home+buildUpPlayPassing_home+buildUpPlayPassingClass_home+
                   buildUpPlayPositioningClass_home+chanceCreationPassing_home+chanceCreationPassingClass_home+
                   chanceCreationCrossing_home+chanceCreationCrossingClass_home+chanceCreationShooting_home+
                   chanceCreationShootingClass_home+chanceCreationPositioningClass_home+defencePressure_home+
                   defencePressureClass_home+defenceAggression_home+defenceAggressionClass_home+
                   defenceTeamWidth_home+defenceTeamWidthClass_home+defenceDefenderLineClass_home+
                   buildUpPlaySpeed1_away+buildUpPlaySpeedClass1_away+
                   buildUpPlayDribblingClass1_away+buildUpPlayPassing1_away+buildUpPlayPassingClass1_away+
                   buildUpPlayPositioningClass1_away+chanceCreationPassing1_away+chanceCreationPassingClass1_away+
                   chanceCreationCrossing1_away+chanceCreationCrossingClass1_away+chanceCreationShooting1_away+
                   chanceCreationShootingClass1_away+chanceCreationPositioningClass1_away+defencePressure1_away+
                   defencePressureClass1_away+defenceAggression1_away+defenceAggressionClass1_away+
                   defenceTeamWidth1_away+defenceTeamWidthClass1_away+defenceDefenderLineClass1_away, data=soccer)


###    save the TE values for all models in all $B=100$ loops
B= 100;            ### number of loops
TEALL = NULL;      ### Final TE values
n = dim(soccer_nona)[1]
n1 = round(n/5)

for (b in 1:B){
  ### randomly select 25 observations as testing data in each loop
  flag <- sort(sample(1:n, n1));
  train <- soccer_nona[-flag,];
  test  <- soccer_nona[flag,];
  model <- multinom(outcome ~ country_id+season+stage+match_api_id+home_team_api_id+
                     away_team_api_id+Goal_Diff_Home_10.Games+Goal_Diff_away_10.Games+
                     B365H+B365D+B365A+BWH+BWD+BWA+IWH+IWD+IWA+LBH+LBD+LBA+PSH+PSD+PSA+
                     WHH+WHD+WHA+SJH+SJD+SJA+VCH+VCD+VCA+GBH+GBD+GBA+BSH+BSD+BSA+
                     buildUpPlaySpeed_home+buildUpPlaySpeedClass_home+
                     buildUpPlayDribblingClass_home+buildUpPlayPassing_home+buildUpPlayPassingClass_home+
                     buildUpPlayPositioningClass_home+chanceCreationPassing_home+chanceCreationPassingClass_home+
                     chanceCreationCrossing_home+chanceCreationCrossingClass_home+chanceCreationShooting_home+
                     chanceCreationShootingClass_home+chanceCreationPositioningClass_home+defencePressure_home+
                     defencePressureClass_home+defenceAggression_home+defenceAggressionClass_home+
                     defenceTeamWidth_home+defenceTeamWidthClass_home+defenceDefenderLineClass_home+
                     buildUpPlaySpeed1_away+buildUpPlaySpeedClass1_away+
                     buildUpPlayDribblingClass1_away+buildUpPlayPassing1_away+buildUpPlayPassingClass1_away+
                     buildUpPlayPositioningClass1_away+chanceCreationPassing1_away+chanceCreationPassingClass1_away+
                     chanceCreationCrossing1_away+chanceCreationCrossingClass1_away+chanceCreationShooting1_away+
                     chanceCreationShootingClass1_away+chanceCreationPositioningClass1_away+defencePressure1_away+
                     defencePressureClass1_away+defenceAggression1_away+defenceAggressionClass1_away+
                     defenceTeamWidth1_away+defenceTeamWidthClass1_away+defenceDefenderLineClass1_away, 
                    data=train)
  #Testmod <- mean(predict(model,test[,1:8]) != test$outcome)
  Testmod <- mean(predict(model,test) != test$outcome)
  #Testmod <- mean(predict(model,test[,1:79]) != test$outcome)
  mean(predict(multinom(outcome~., soccer_nona, na.action=na.omit)) != test$outcome)
  TEALL = rbind(TEALL, cbind(Testmod))
}
