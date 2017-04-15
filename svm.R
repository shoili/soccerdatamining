library(dplyr)
library(e1071)

soccer_nona <- read.csv("soccer_nona.csv", strip.white = T)
###    save the TE values for all models in all $B=100$ loops
###    save the TE values for all models in all $B=100$ loops
B= 100;            ### number of loops
TEALL = NULL;      ### Final TE values
n = dim(soccer_nona)[1]
n1 = round(n/5)

soccer_nona <- soccer_nona[c(0, 5, 6, 7, 8, 79)]
# x <- soccer_nona[c(0,5,6,7,8)]
# y <- as.factor(soccer_nona$outcome)

flag <- sort(sample(1:n, n1));
train <- soccer_nona[-flag,];
test  <- soccer_nona[flag,];

xtrain <- train[c(1:4)]
xtest <- test[c(1:4)]
ytrain <- as.factor(train$outcome)
ytest <- as.factor(test$outcome)
# model <- svm(outcome ~ away_team_api_id+Goal_Diff_Home_10.Games+Goal_Diff_away_10.Games,
#                   data=train)

model <- svm(model, xtest)
Testmod <- mean(predict(model, xtest) != ytest)
