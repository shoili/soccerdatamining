setwd("C:\\Users\\Parit\\Desktop\\Classwork\\Georgia Tech\\Spring 2017 work\\ISyE 7406 - Data Mining and Statistical Learning\\Project")
data <- read.csv('soccer_4_14.csv',header=TRUE)
y <- data$home_team_goal - data$away_team_goal
y[y>0] <- 1
y[y<0] <- -1
data1<-data
data[,c('home_team_goal','away_team_goal','Goal_Diff_away','Goal_Diff_Home','outcome',
        'id', 'country_id', 'match_api_id', 'home_team_api_id')]<-NULL
data[,c('home_team_goal','away_team_goal','Goal_Diff_away','Goal_Diff_Home','outcome',
        'id', 'country_id', 'match_api_id', 'home_team_api_id','away_team_api_id','id1_home_home',
        'team_fifa_api_id_home_home', 'id2_away_away', 'team_api_id1_away_away',
        'team_fifa_api_id1_away_away', 'id5','date','team_short_name_home_home','team_short_name1_away_away',  
        'id6_away'   ,'team_api_id3_away' ,'date2_away','date1_home' ,'outcome',
        "team_long_name1_away_away","team_long_name_home_home")]<-NULL
datanobet <- data
datanobet <- datanobet[""]
filternull<-data[ , apply(data, 2, function(x) !any(is.na(x)))]
is.fact <- sapply(filternull, is.factor)
cat<- filternull[is.fact]
# filternull$Goal_Diff_Home_10.Games <- as.numeric(filternull$Goal_Diff_Home_10.Games)
# filternull$shoton <- as.numeric(filternull$shoton)
# filternull$shotoff <- as.numeric(filternull$shotoff)
# filternull$foulcommit <- as.numeric(filternull$foulcommit)
# filternull$card <- as.numeric(filternull$card)
# filternull$possession <- as.numeric(filternull$possession)
# filternull$corner <- as.numeric(filternull$corner)
# filternull$cross <- as.numeric(filternull$cross)

filternull$stage <- as.factor(filternull$stage)
filternull$country_id <- as.factor(filternull$country_id)



y<-as.factor(y)
filternull['y']<-as.factor(y)
data['y']<-as.factor(y)
epl <-filternull[filternull$League_name == "England Premier League",]
yepl <-y[filternull$League_name == "England Premier League"]


### Train test partitioning
n = dim(data)[1]; ### total number of observations
n1 = round(n/5); ### number of observations randomly selected for testing data### set the random seed
flag = sort(sample(1:n, n1));
train = data[-flag,]; test = data[flag,];
ytr = y[-flag]; yt = y[flag];
trainf = filternull[-flag,]; testf = filternull[flag,];


ne = dim(epl)[1]; ### total number of observations
ne1 = round(ne/5); ### number of observations randomly selected for testing data### set the random seed
flage = sort(sample(1:ne, ne1));
traine = epl[-flage,]; teste = epl[flage,];
ytre = yepl[-flage]; yte = yepl[flage];


library(randomForest)

fit <- randomForest( y~ .,
                    data=traine, 
                    importance=TRUE, 
                    ntree=100)
predtrain <- mean(predict(fit,traine) == ytre)
predtest <- mean(predict(fit,teste) == yte)

fit <- randomForest( y ~ Goal_Diff_away_10.Games + Goal_Diff_Home_10.Games + buildUpPlayPositioningClass_home + 
                       buildUpPlayPositioningClass1_away + chanceCreationPositioningClass_home + 
                       chanceCreationPositioningClass1_away + defenceAggression_home + 
                       chanceCreationShooting_home + chanceCreationShootingClass_home + 
                       chanceCreationShooting1_away + chanceCreationShootingClass1_away + 
                       buildUpPlayPassing_home + chanceCreationPassing_home + chanceCreationPassing1_away + 
                       stage + defenceDefenderLineClass_home + buildUpPlayPassingClass_home + 
                       chanceCreationCrossing1_away + defenceAggression1_away + 
                       Country_Name_home,
                     data=trainf, 
                     importance=TRUE, 
                     ntree=500)
predtrainf <- mean(predict(fit,trainf) == ytr)
predtestf <- mean(predict(fit,testf) == yt)

library(party)
fitp <- cforest(y~.,
               data = train, 
               controls=cforest_unbiased(ntree=100, mtry=3))
Prediction <- mean(predict(fitp, test, OOB=TRUE, type = "response") ==y)


#entropy
library(FSelector)

weights <- information.gain(y~., data)
print(weights)
subset <- cutoff.k(weights, 20)
f <- as.simple.formula(subset, "y")
print(f)

weights <- information.gain(y~., filternull)
print(weights)
subset <- cutoff.k(weights, 20)
f <- as.simple.formula(subset, "y")
print(f)

write.csv(weights,"dataingfogainnobets.csv")
