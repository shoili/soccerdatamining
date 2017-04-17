library(dplyr)
library(e1071)

soccer <- read.csv("soccer_nona.csv", strip.white = T)
# soccer$home_team_api_id <- as.factor(soccer$home_team_api_id)
# soccer$away_team_api_id <- as.factor(soccer$away_team_api_id)
###    save the TE values for all models in all $B=100$ loops

# soccer_nona <- soccer[c(0, 7, 8, 9:38, 39, 42, 45, 47, 49, 52, 54, 56, 59, 62, 65, 67, 69, 72, 74, 76, 79)]
soccer_nona <- soccer[c(0, 7, 8, 39, 42, 45, 47, 49, 52, 54, 56, 59, 62, 65, 67, 69, 72, 74, 76, 79)]

#epl_nobet_numeric_train <- train[c(4,5,6,9,12,14,16,19,21,23,26,29,32,34,36,39,41,43,50)]
#epl_nobet_numeric_test <- test[c(4,5,6,9,12,14,16,19,21,23,26,29,32,34,36,39,41,43,50)]

#epl_bet_numeric_train <- train[c(4:15,18,21,23,25,28,30,32,35,38,41,43,45,48,50,52,59)]
#epl_bet_numeric_test <- test[c(4:15,18,21,23,25,28,30,32,35,38,41,43,45,48,50,52,59)]

B= 10;            ### number of loops
TEALL = NULL;      ### Final TE values
n = dim(soccer_epl_nonan)[1]
n1 = round(n/10)

############### SVM ############################
for (b in 1:B){

flag <- sort(sample(1:n, n1));
train <- soccer_epl_nonan[-flag,];
test  <- soccer_epl_nonan[flag,];

xtrain <- train[c(4:15,18,21,23,25,28,30,32,35,38,41,43,45,48,50,52,59)]
xtest <- test[c(4:15,18,21,23,25,28,30,32,35,38,41,43,45,48,50,52,59)]
ytrain <- as.factor(train$outcome)
ytest <- as.factor(test$outcome)

## MULTICLASS SVM
model <- svm(xtrain, ytrain, gamma = 0.1, kernel = 'radial') 
Test_svm <- mean(predict(model, xtest) == ytest)
TEALL4 = rbind(TEALL, cbind(Test_svm))
}

final_test_svm_wins <- mean(TEALL) #epl_nobet: 0.634583, epl_bet: 0.6491667
final_test_svm_loss <- mean(TEALL2) #epl_nobet: 0.644697, epl_bet: 0.6587121
final_test_svm_tie <- mean(TEALL3) #epl_nobet: 0.6435606, epl_bet: 0.6568182
final_test_svm_outcome <- mean(TEALL4) #epl_nobet: 0.6257576, epl_bet: 0.633333

############### Multinom ############################
library(nnet)
B=100
TEALL4_multi = NULL;      ### Final TE values
for (b in 1:B){
  
  flag <- sort(sample(1:n, n1));
  train <- soccer_epl_nobet[-flag,]; #train <- soccer_epl_nonan[-flag,];
  test  <- soccer_epl_nobet[flag,]; #test  <- soccer_epl_nonan[flag,];
  model <- multinom(tie ~ country_id+season+stage+Goal_Diff_Home_10.Games+Goal_Diff_away_10.Games+
                      #B365H+B365D+B365A+WHH+WHD+WHA+VCH+VCD+VCA+ #take line out if using soccer_epl_nobet
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
                      defenceTeamWidth1_away+defenceTeamWidthClass1_away+defenceDefenderLineClass1_away+goal_diff_diff,
                    data=train)
  Testmulti <- mean(predict(model, test[,c(1:45,50)]) == test$tie) #test[,c(1:45,50)], test[,c(1:54,59)]
  TEALL4_multi = rbind(Testmulti, cbind(Testmulti))
}
final_test_multinom_outcome <- mean(TEALL_multi) #epl_nobet: 0.4208333, 
final_test_multinom_win <- mean(TEALL2_multi) #epl_nobet: 0.6791667
final_test_multinom_loss <- mean(TEALL3_multi) #epl_nobet: 0.7 
final_test_multinom_tie <- mean(TEALL4_multi) #epl_nobet: 0.675
