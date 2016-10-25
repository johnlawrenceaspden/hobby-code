#!/usr/bin/r

library(rpart)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

## Try again with the child and Fare2 bins that were helpful previously
train <- read.csv("../train.csv", stringsAsFactors=FALSE)
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
test <- read.csv("../test.csv", stringsAsFactors=FALSE)
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
write.csv(submit, file = "0.79904-decisiontreewithextrafeatures.csv", row.names = FALSE)
