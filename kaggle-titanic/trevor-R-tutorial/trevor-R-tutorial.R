## http://trevorstephens.com/kaggle-titanic-tutorial/r-part-1-booting-up/


## if we read in the training file with read.csv, all strings will get read as factors
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


## better to turn this off and then factorize the categorical variables by hand
train <- read.csv("train.csv", stringsAsFactors=FALSE)
train$Pclass=factor(train$Pclass) # not clear whether this should be integer or factor
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


# Of the 891 passengers in our training data, only 342 survived
table(train$Survived)

## 0   1 
## 549 342 

# Or 38% survived, 62% died
prop.table(table(train$Survived))

## 0         1 
## 0.6161616 0.3838384 


## So our first prediction is that everybody dies
test <- read.csv("test.csv", stringsAsFactors=FALSE)
test$Survived <- rep(0,418)
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

## Submission of theyallperish.csv to Kaggle yields a score of 0.62679
## very close to the 0.61616 performance of this model on the test set




## http://trevorstephens.com/kaggle-titanic-tutorial/r-part-2-the-gender-class-model/

## In our training set there are 314 females and 577 males
summary(train$Sex)
## female   male 
##    314    577 

## Most of the men died, most of the women lived
table(train$Sex, train$Survived)
##          0   1
## female  81 233
## male   468 109

## 74% of the women, but only 18% of the men
prop.table(table(train$Sex, train$Survived),1)        
##                0         1
## female 0.2579618 0.7420382
## male   0.8110919 0.1889081


test <- read.csv("test.csv", stringsAsFactors=FALSE)
test$Survived <- 0
test$Survived[test$Sex=='female'] <- 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "gendermodel.csv", row.names = FALSE)

## Taking account of sex increases our submission score to 0.76555

## paranoid check
prop.table(table(test$Sex, test$Survived),1)
##        0 1
## female 0 1
## male   1 0


## We can also work out our performance on the training set 
train$Predict <- 0
train$Predict[train$Sex=='female'] <- 1
prop.table(table(train$Predict==train$Survived))
##     FALSE      TRUE 
## 0.2132435 0.7867565
## The performance on the training set is: 0.78675


## As everyone knows, "Women and Children First" 


summary(train$Age)
   ## Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   ## 0.42   20.12   28.00   29.70   38.00   80.00     177 
hist(train$Age)

  


train$Child <- 0
train$Child[train$Age < 10]<-1

aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=train, FUN=length)

aggregate(Survived ~ Child + Sex, data=train, FUN=function(x){sum(x)/length(x)})
aggregate(Survived ~ Child + Sex + Pclass, data=train, FUN=function(x){sum(x)/length(x)})

# Predict that females and under-tens make it
train$Predict <- 0
train$Predict[train$Sex=='female' | train$Child==1] <- 1

prop.table(table(train$Predict==train$Survived))
##     FALSE      TRUE 
## 0.2065095 0.7934905 

# Predict that females and under-tens make it
test <- read.csv("test.csv", stringsAsFactors=FALSE)
test$Child[test$Age < 10] <- 1
test$Survived <- 0
test$Survived[test$Sex=='female' | test$Child==1] <- 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "femalesandundertens.csv", row.names = FALSE)
## scores 0.77033


## Now look at the fares paid, split into 4 bins <10,10-20,20-30,30+
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

## try it the other way round, say that female and children live but not if they're in
## the fatal 3rd class and over £20
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


## Part 3, decision trees
## http://trevorstephens.com/kaggle-titanic-tutorial/r-part-3-decision-trees/


train <- read.csv("train.csv", stringsAsFactors=FALSE)
train$Pclass=factor(train$Pclass)
train$Sex=factor(train$Sex)
train$Embarked=factor(train$Embarked)
str(train)

 ## $ PassengerId: int  1 2 3 4 5 6 7 8 9 10 ...
 ## $ Survived   : int  0 1 1 1 0 0 0 0 1 1 ...
 ## $ Pclass     : Factor w/ 3 levels "1","2","3": 3 1 3 1 3 3 1 3 3 2 ...
 ## $ Name       : chr  "Braund, Mr. Owen Harris" "Cumings, Mrs. John Bradley (Florence Briggs Thayer)" "Heikkinen, Miss. Laina" "Futrelle, Mrs. Jacques Heath (Lily May Peel)" ...
 ## $ Sex        : Factor w/ 2 levels "female","male": 2 1 1 1 2 2 2 2 1 1 ...
 ## $ Age        : num  22 38 26 35 35 NA 54 2 27 14 ...
 ## $ SibSp      : int  1 1 0 1 0 0 0 3 0 1 ...
 ## $ Parch      : int  0 0 0 0 0 0 0 1 2 0 ...
 ## $ Ticket     : chr  "A/5 21171" "PC 17599" "STON/O2. 3101282" "113803" ...
 ## $ Fare       : num  7.25 71.28 7.92 53.1 8.05 ...
 ## $ Cabin      : chr  "" "C85" "" "C123" ...
 ## $ Embarked   : Factor w/ 4 levels "","C","Q","S": 4 2 4 4 4 3 4 4 4 2 ...

library(rpart)

fit <- rpart(Survived ~ Sex, data=train, method="class")
plot(fit)
text(fit)

## Can't get RGtk2, but there's a debian package for it
## install.packages('RGtk2', dep=TRUE)
## sudo apt install r-cran-rgtk2
## install.packages('rattle')
## install.packages('rpart.plot')
## install.packages('RColorBrewer')

library(rattle)
library(rpart.plot)
library(RColorBrewer)

## With these loaded we can make fancy decision tree pictures
fit <- rpart(Survived ~ Sex, data=train, method="class")
fancyRpartPlot(fit)

fit <- rpart(Survived ~ Sex + Pclass , data=train, method="class")
fancyRpartPlot(fit)



## Let's create a decision tree for all the variables we were given
fit <- rpart(Survived ~ Sex + Pclass + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
fancyRpartPlot(fit)

test <- read.csv("test.csv", stringsAsFactors=FALSE)
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstdecisiontree.csv", row.names = FALSE)
## scores 0.79426, which is the same as the best hand model above
## femalesandundertensbutnothighpayingthirdclass.csv
0.79426*nrow(test) ## 332 correct predictions


## Try again with the child and Fare2 bins that were helpful previously
train <- read.csv("train.csv", stringsAsFactors=FALSE)
train$Pclass=factor(train$Pclass)
train$Sex=factor(train$Sex)
train$Embarked=factor(train$Embarked)
train$Child <- 0
train$Child[train$Age < 10]<-1
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

fit <- rpart(Survived ~ Sex + Pclass + Age + SibSp + Parch + Fare + Embarked + Child + Fare2, data=train, method="class")
fancyRpartPlot(fit)

## Same factors and bins for test
test <- read.csv("test.csv", stringsAsFactors=FALSE)
test$Pclass=factor(test$Pclass)
test$Sex=factor(test$Sex)
test$Embarked=factor(test$Embarked)
test$Child <- 0
test$Child[test$Age < 10]<-1
test$Fare2 <- '30+'
test$Fare2[test$Fare < 30 & test$Fare >= 20] <- '20-30'
test$Fare2[test$Fare < 20 & test$Fare >= 10] <- '10-20'
test$Fare2[test$Fare < 10] <- '<10'
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived=Prediction)
write.csv(submit, file = "decisiontreewithextrafeatures.csv", row.names = FALSE)

# This is the best so far with a score of 0.79904

nrow(test) ##418
0.79904*nrow(test) ## 333.9987
## I.e. it will correctly predict the fate of 334 of the 418 passengers in test


## Overfitting
fit <- rpart(Survived ~ Sex + Pclass + Age + SibSp + Parch + Fare + Embarked + Child + Fare2, data=train, method="class", control=rpart.control(minsplit=2,cp=0))
fancyRpartPlot(fit)
## A very large tree indeed

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "overfit.csv", row.names = FALSE)

## Overfitting collapses score to 0.73206, or 306 correct predictions
0.73206*nrow(test)


## Let's remove the Fare and Age variables from the classifier
fit <- rpart(Survived ~ Sex + Pclass + SibSp + Parch + Embarked + Child + Fare2, data=train, method="class", control=rpart.control())
fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "decisiontreeagebinnednofare.csv", row.names = FALSE)

## Looked sensible (using embarkation Southhampton instead of high
## fare) but only managed 0.73206. Hmmm...

## Hand-trimming the tree
fit <- rpart(Survived ~ Sex + Pclass + Age + SibSp + Parch + Fare + Embarked + Child + Fare2, data=train, method="class", control=rpart.control(minsplit=5,cp=0.01))
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)













###################################################
## Notes

## There is something funny going on with children
## It looks like if you've a fractional age then you're certain to live
## Perhaps the exact ages of babies were only recorded afterwards
library(ggplot2)
library(ggthemes)

fluff=train[train$Age<2 & !is.na(train$Age),]
ggplot(fluff, aes(x = Age, fill = factor(Survived)))+geom_bar(stat='count')+scale_x_continuous(breaks=c(1:18)) +theme_few()

## Some handy bins for the continuous variables
train$Child <- 0
train$Child[train$Age < 10]<-1

train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
