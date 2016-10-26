#!/usr/bin/r

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)


train <- read.csv("../train.csv", stringsAsFactors=FALSE)
test <- read.csv("../test.csv", stringsAsFactors=FALSE)
test$Survived <- NA
combi<-rbind(train,test)

## Children are those under 10
combi$Child <- 0
combi$Child[combi$Age < 10]<-1

## Bin the fares
combi$Fare2 <- '30+'
combi$Fare2[combi$Fare < 30 & combi$Fare >= 20] <- '20-30'
combi$Fare2[combi$Fare < 20 & combi$Fare >= 10] <- '10-20'
combi$Fare2[combi$Fare < 10] <- '<10'

## Pull out a title feature
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)

## combine Madame and Mademoiselle to Mlle 
combi$Title[combi$Title == 'Mme'] <- 'Mrs'
combi$Title[combi$Title == 'Mlle'] <- 'Miss'


## Male nobility to Sir
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Jonkheer')] <- 'Sir'

## Female Nobility to Lady
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'

combi$Title <- factor(combi$Title)

## changing various columns to factors doesn't seem to make any difference
## combi$Pclass=factor(combi$Pclass)
## combi$Sex=factor(combi$Sex)
## combi$Embarked=factor(combi$Embarked)
## combi$Child <- as.factor(combi$Child)
## combi$Fare2 <- as.factor(combi$Fare2)

train <- combi[1:891,]
test <- combi[892:1309,]


fit <- rpart(Survived ~ Sex + Pclass + Age + SibSp + Parch + Fare + Embarked + Child + Fare2 + Title, data=train, method="class")
fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived=Prediction)
write.csv(submit, file = "0.80383-decisiontreewithextrafeatures.csv", row.names = FALSE)
