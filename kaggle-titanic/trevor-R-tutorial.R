## http://trevorstephens.com/kaggle-titanic-tutorial/r-part-1-booting-up/

train <- read.csv("train.csv")
str(train)

## 'data.frame':	891 obs. of  12 variables:
##  $ PassengerId: int  1 2 3 4 5 6 7 8 9 10 ...
##  $ Survived   : int  0 1 1 1 0 0 0 0 1 1 ...
##  $ Pclass     : int  3 1 3 1 3 3 1 3 3 2 ...
##  $ Name       : Factor w/ 891 levels "Abbing, Mr. Anthony",..: 109 191 358 277 16 559 520 629 416 581 ...
##  $ Sex        : Factor w/ 2 levels "female","male": 2 1 1 1 2 2 2 2 1 1 ...
##  $ Age        : num  22 38 26 35 35 NA 54 2 27 14 ...
##  $ SibSp      : int  1 1 0 1 0 0 0 3 0 1 ...
##  $ Parch      : int  0 0 0 0 0 0 0 1 2 0 ...
##  $ Ticket     : Factor w/ 681 levels "110152","110413",..: 525 596 662 50 473 276 86 396 345 133 ...
##  $ Fare       : num  7.25 71.28 7.92 53.1 8.05 ...
##  $ Cabin      : Factor w/ 148 levels "","A10","A14",..: 1 83 1 57 1 1 131 1 1 1 ...
##  $ Embarked   : Factor w/ 4 levels "","C","Q","S": 4 2 4 4 4 3 4 4 4 2 ...

train <- read.csv("train.csv", stringsAsFactors=FALSE)
str(train)

## 'data.frame':	891 obs. of  12 variables:
##  $ PassengerId: int  1 2 3 4 5 6 7 8 9 10 ...
##  $ Survived   : int  0 1 1 1 0 0 0 0 1 1 ...
##  $ Pclass     : int  3 1 3 1 3 3 1 3 3 2 ...
##  $ Name       : chr  "Braund, Mr. Owen Harris" "Cumings, Mrs. John Bradley (Florence Briggs Thayer)" "Heikkinen, Miss. Laina" "Futrelle, Mrs. Jacques Heath (Lily May Peel)" ...
##  $ Sex        : chr  "male" "female" "female" "female" ...
##  $ Age        : num  22 38 26 35 35 NA 54 2 27 14 ...
##  $ SibSp      : int  1 1 0 1 0 0 0 3 0 1 ...
##  $ Parch      : int  0 0 0 0 0 0 0 1 2 0 ...
##  $ Ticket     : chr  "A/5 21171" "PC 17599" "STON/O2. 3101282" "113803" ...
##  $ Fare       : num  7.25 71.28 7.92 53.1 8.05 ...
##  $ Cabin      : chr  "" "C85" "" "C123" ...
##  $ Embarked   : chr  "S" "C" "S" "S" ...

train$Pclass=factor(train$Pclass)
train$Sex=factor(train$Sex)
train$Embarked=factor(train$Embarked)

str(train)
## 'data.frame':	891 obs. of  12 variables:
##  $ PassengerId: int  1 2 3 4 5 6 7 8 9 10 ...
##  $ Survived   : int  0 1 1 1 0 0 0 0 1 1 ...
##  $ Pclass     : Factor w/ 3 levels "1","2","3": 3 1 3 1 3 3 1 3 3 2 ...
##  $ Name       : chr  "Braund, Mr. Owen Harris" "Cumings, Mrs. John Bradley (Florence Briggs Thayer)" "Heikkinen, Miss. Laina" "Futrelle, Mrs. Jacques Heath (Lily May Peel)" ...
##  $ Sex        : Factor w/ 2 levels "female","male": 2 1 1 1 2 2 2 2 1 1 ...
##  $ Age        : num  22 38 26 35 35 NA 54 2 27 14 ...
##  $ SibSp      : int  1 1 0 1 0 0 0 3 0 1 ...
##  $ Parch      : int  0 0 0 0 0 0 0 1 2 0 ...
##  $ Ticket     : chr  "A/5 21171" "PC 17599" "STON/O2. 3101282" "113803" ...
##  $ Fare       : num  7.25 71.28 7.92 53.1 8.05 ...
##  $ Cabin      : chr  "" "C85" "" "C123" ...
##  $ Embarked   : Factor w/ 4 levels "","C","Q","S": 4 2 4 4 4 3 4 4 4 2 ...



table(train$Survived)

## 0   1 
## 549 342 

prop.table(table(train$Survived))

## 0         1 
## 0.6161616 0.3838384 

test <- read.csv("test.csv", stringsAsFactors=FALSE)

## predict that everybody dies

test$Survived <- rep(0,418)

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)

write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

## Submission of theyallperish.csv to Kaggle yields a score of 0.62679
## very close to the 0.61616 performance on the test set

## http://trevorstephens.com/kaggle-titanic-tutorial/r-part-2-the-gender-class-model/

summary(train$Sex)

table(train$Sex, train$Survived)

##          0   1
## female  81 233
## male   468 109

prop.table(table(train$Sex, train$Survived),1)        

##                0         1
## female 0.2579618 0.7420382
## male   0.8110919 0.1889081

test$Survived <- 0
test$Survived[test$Sex=='female'] <- 1

prop.table(table(test$Sex, test$Survived),1)
##        0 1
## female 0 1
## male   1 0


submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)

write.csv(submit, file = "gendermodel.csv", row.names = FALSE)

## Submission score 0.76555
## Performance on training set is: 0.78675

train$Predict <- 0
train$Predict[train$Sex=='female'] <- 1

prop.table(table(train$Predict==train$Survived))
##     FALSE      TRUE 
## 0.2132435 0.7867565


summary(train$Age)
   ## Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   ## 0.42   20.12   28.00   29.70   38.00   80.00     177 

train$Child <- 0
train$Child[train$Age < 10]<-1

aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=train, FUN=length)

aggregate(Survived ~ Child + Sex, data=train, FUN=function(x){sum(x)/length(x)})

# Predict that females and under-tens make it
train$Predict <- 0
train$Predict[train$Sex=='female' | train$Child==1] <- 1

prop.table(table(train$Predict==train$Survived))
##     FALSE      TRUE 
## 0.2065095 0.7934905 


test <- read.csv("test.csv", stringsAsFactors=FALSE)

test$Child[test$Age < 10] <- 1
test$Survived <- 0
test$Survived[test$Sex=='female' | test$Child==1] <- 1

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)

write.csv(submit, file = "femalesandundertens.csv", row.names = FALSE)

## scores 0.77033


train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

aggregate(Survived ~ Child + Sex + Pclass + Fare2, data=train, FUN=function(x){sum(x)/length(x)})

aggregate(Survived ~ Child + Sex + Pclass, data=train, FUN=function(x){sum(x)/length(x)})

aggregate(Survived ~ Sex + Pclass + Fare2, data=train, FUN=function(x){sum(x)/length(x)})



test <- read.csv("test.csv", stringsAsFactors=FALSE)

test$Child[test$Age < 10] <- 1
test$Survived <- 0
test$Survived[test$Sex=='female'] <- 1
test$Survived[test$Pclass==3 & test$Fare >=20] <- 0
test$Survived[test$Child==1] <- 1

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)

write.csv(submit, file = "femalesbutnothighpayingthirdclassandundertens.csv", row.names = FALSE)

## scores 0.77990, which is the same as ignoring children! 2847 on leaderboard

test <- read.csv("test.csv", stringsAsFactors=FALSE)

test$Child[test$Age < 10] <- 1
test$Survived <- 0
test$Survived[test$Sex=='female'] <- 1
test$Survived[test$Child==1] <- 1
test$Survived[test$Pclass==3 & test$Fare >=20] <- 0

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)

write.csv(submit, file = "femalesandundertensbutnothighpayingthirdclass.csv", row.names = FALSE)
## Wow! 0.79426, moves me from 2847 to 1407 on leaderboard
## Something is killing high paying third class passengers including women and children.
## Perhaps they were trapped somehow?


