#!/usr/bin/r

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)


train <- read.csv("../train.csv", stringsAsFactors=FALSE)
test <- read.csv("../test.csv", stringsAsFactors=FALSE)
test$Survived <- NA
combi<-rbind(train,test)


combi$Pclass=factor(combi$Pclass)
combi$Sex=factor(combi$Sex)
combi$Embarked=factor(combi$Embarked)
combi$Child <- 0
combi$Child[combi$Age < 10]<-1
combi$Fare2 <- '30+'
combi$Fare2[combi$Fare < 30 & combi$Fare >= 20] <- '20-30'
combi$Fare2[combi$Fare < 20 & combi$Fare >= 10] <- '10-20'
combi$Fare2[combi$Fare < 10] <- '<10'

train <- combi[1:891,]
test <- combi[892:1309,]


fit <- rpart(Survived ~ Sex + Pclass + Age + SibSp + Parch + Fare + Embarked + Child + Fare2, data=train, method="class")
fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived=Prediction)
write.csv(submit, file = "0.79904-decisiontreewithextrafeatures.csv", row.names = FALSE)
