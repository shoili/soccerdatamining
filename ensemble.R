# Ensemble method
B=10
TEALL_ensem2 = NULL
for (b in 1:B) {
flag <- sort(sample(1:n, n1));
train <- soccer_epl_nobet[-flag,]; #train <- soccer_epl_nonan[-flag,];
test  <- soccer_epl_nobet[flag,]; #test  <- soccer_epl_nonan[flag,];
model <- multinom(win ~ country_id+season+stage+Goal_Diff_Home_10.Games+Goal_Diff_away_10.Games+
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
win_pred <- predict(model, soccer_epl_nobet[,c(1:45,50)])
nowin_pred_index <- which(win_pred == 0)
# nowin_pred_index <- which(win_pred != 0)

soccer_epl_nobet_winloss <- soccer_epl_nobet[nowin_pred_index, ]

# model2 <- multinom(lose ~ country_id+season+stage+Goal_Diff_Home_10.Games+Goal_Diff_away_10.Games+
#                     buildUpPlaySpeed_home+buildUpPlaySpeedClass_home+
#                     buildUpPlayDribblingClass_home+buildUpPlayPassing_home+buildUpPlayPassingClass_home+
#                     buildUpPlayPositioningClass_home+chanceCreationPassing_home+chanceCreationPassingClass_home+
#                     chanceCreationCrossing_home+chanceCreationCrossingClass_home+chanceCreationShooting_home+
#                     chanceCreationShootingClass_home+chanceCreationPositioningClass_home+defencePressure_home+
#                     defencePressureClass_home+defenceAggression_home+defenceAggressionClass_home+
#                     defenceTeamWidth_home+defenceTeamWidthClass_home+defenceDefenderLineClass_home+
#                     buildUpPlaySpeed1_away+buildUpPlaySpeedClass1_away+
#                     buildUpPlayDribblingClass1_away+buildUpPlayPassing1_away+buildUpPlayPassingClass1_away+
#                     buildUpPlayPositioningClass1_away+chanceCreationPassing1_away+chanceCreationPassingClass1_away+
#                     chanceCreationCrossing1_away+chanceCreationCrossingClass1_away+chanceCreationShooting1_away+
#                     chanceCreationShootingClass1_away+chanceCreationPositioningClass1_away+defencePressure1_away+
#                     defencePressureClass1_away+defenceAggression1_away+defenceAggressionClass1_away+
#                     defenceTeamWidth1_away+defenceTeamWidthClass1_away+defenceDefenderLineClass1_away+goal_diff_diff,
#                   data=soccer_epl_nobet_winloss)

xtrain <- soccer_epl_nobet_winloss[c(4,5,6,9,12,14,16,19,21,23,26,29,32,34,36,39,41,43,50)]
# xtest <- test[c(4,5,6,9,12,14,16,19,21,23,26,29,32,34,36,39,41,43,50)]
ytrain <- soccer_epl_nobet_winloss$lose[nowin_pred_index]
# ytrain <- as.factor(win_pred)
# ytest <- as.factor(test$outcome)

## MULTICLASS SVM
model2 <- svm(xtrain, ytrain, gamma = 0.1, kernel = 'radial') 

loss_pred <- predict(model2, xtrain)
#loss_pred <- predict(model2, soccer_epl_nobet_winloss[,c(1:45,50)])
#mean(loss_pred == soccer_epl_nobet_winloss$lose)

winprednum <- as.numeric(win_pred)-1 ## 1 = win. 0 = notwin
lossprednum <- 4 - as.numeric(loss_pred)  ## 2 == loss. 3 == tie

for (i in 1:length(nowin_pred_index)){
  # print(winprednum[nowin_pred_index[i]])
  # print(lossprednum[i])
  winprednum[nowin_pred_index[i]] = lossprednum[i]
}

perf = mean(winprednum == soccer_epl_nobet$outcome)
TEALL_ensem2 = c(TEALL_ensem, perf)
}

#ensem_logit <- mean(TEALL_ensem) #0.5046578
ensem_svm <- mean(TEALL_ensem)