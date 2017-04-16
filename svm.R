library(dplyr)
library(e1071)

soccer <- read.csv("soccer_nona.csv", strip.white = T)
# soccer$home_team_api_id <- as.factor(soccer$home_team_api_id)
# soccer$away_team_api_id <- as.factor(soccer$away_team_api_id)
###    save the TE values for all models in all $B=100$ loops

B= 100;            ### number of loops
TEALL = NULL;      ### Final TE values
n = dim(soccer_nona)[1]
n1 = round(n/5)

# soccer_nona <- soccer[c(0, 7, 8, 9:38, 39, 42, 45, 47, 49, 52, 54, 56, 59, 62, 65, 67, 69, 72, 74, 76, 79)]
soccer_nona <- soccer[c(0, 7, 8, 39, 42, 45, 47, 49, 52, 54, 56, 59, 62, 65, 67, 69, 72, 74, 76, 79)]

flag <- sort(sample(1:n, n1));
train <- soccer_nona[-flag,];
test  <- soccer_nona[flag,];

xtrain <- train[c(1:4)]
xtest <- test[c(1:4)]
ytrain <- as.factor(train$outcome)
ytest <- as.factor(test$outcome)

## MULTICLASS SVM
model <- svm(xtrain, ytrain, gamma = 0.1, kernel = 'sigmoid') # radial giving .53
mean(predict(model, xtest) != ytest)
