library(dplyr)
library(e1071)
library(pls)

soccer <- read.csv("soccer_nona.csv", strip.white = T)
# soccer <- read.csv("soccer_clean.csv", strip.white = T)

# soccer_nona <- soccer[c(0, 7, 8, 9:38, 39, 42, 45, 47, 49, 52, 54, 56, 59, 62, 65, 67, 69, 72, 74, 76, 79)]
soccer_nona <- soccer[c(0, 7, 8, 39, 42, 45, 47, 49, 52, 54, 56, 59, 62, 65, 67, 69, 72, 74, 76, 79)]

B= 100;            ### number of loops
TEALL = NULL;      ### Final TE values
n = dim(soccer_nona)[1]
n1 = round(n/5)

flag <- sort(sample(1:n, n1));
train <- soccer_nona[-flag,];
test  <- soccer_nona[flag,];

## MULTICLASS SVM

xtrain <- train[c(1:18)]
xtest <- test[c(1:18)]
ytrain <- as.factor(train$outcome)
ytest <- as.factor(test$outcome)

model <- svm(xtrain, ytrain, gamma = 0.1, kernel = 'sigmoid') # radial giving .53
mean(predict(model, xtest) == ytest)

## Two CLASS SVM
# win or not. this sucks
xtrain <- train[c(1:4)]
xtest <- test[c(1:4)]
ytrain <- as.factor(ifelse(train$outcome == 1, 1, 0))
ytest <- as.factor(ifelse(test$outcome == 1, 1, 0))

model <- svm(xtrain, ytrain, gamma = 0.25, kernel = 'sigmoid') # 0.35 with radial and 0.44 with sigmoid
mean(predict(model, xtest) == ytest)

## dim reduction


