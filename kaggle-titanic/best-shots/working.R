#!/usr/bin/r

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)


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

## Make a total Family Size variable
combi$FamilySize <- combi$SibSp + combi$Parch + 1

## Fill in missing variables as in Trevor's tutorial, but making extra _filled variables
Age_filled <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),],
                method="anova")

fancyRpartPlot(Age_filled)

combi$Age_filled<-combi$Age
combi$Age_filled[is.na(combi$Age)] <- predict(Age_filled, combi[is.na(combi$Age),])


which(combi$Embarked=='')
combi$Embarked_filled<-combi$Embarked
combi$Embarked_filled[c(62,830)] = 'S'

which(is.na(combi$Fare))
combi$Fare_filled <- combi$Fare
combi$Fare_filled[1044] <- median(combi$Fare,na.rm=TRUE)

## Bin the fares
combi$Fare2_filled <- '30+'
combi$Fare2_filled[combi$Fare_filled < 30 & combi$Fare_filled >= 20] <- '20-30'
combi$Fare2_filled[combi$Fare_filled < 20 & combi$Fare_filled >= 10] <- '10-20'
combi$Fare2_filled[combi$Fare_filled < 10] <- '<10'


combi$Pclass=factor(combi$Pclass)
combi$Sex=factor(combi$Sex)
combi$Embarked=factor(combi$Embarked)
combi$Embarked_filled=factor(combi$Embarked_filled)
combi$Child <- as.factor(combi$Child)
combi$Fare2 <- as.factor(combi$Fare2)
combi$Fare2_filled <- as.factor(combi$Fare2_filled)
combi$Title <- factor(combi$Title)
combi$Ticket <- factor(combi$Ticket)

train <- combi[1:891,]
test <- combi[892:1309,]

## Fit a Decision Tree using rpart
## adding Family Size actually hurts us
## adding Ticket destroys it!
## Filling in missing Age values hurts us
fit <- rpart(Survived ~ Sex + Pclass + Age + SibSp + Parch + Fare + Embarked + Child + Fare2 + Title, data=train, method="class")
fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived=Prediction)
write.csv(submit, file = "working-rpart.csv", row.names = FALSE)

## Fit a Random Forest using randomForest
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Sex + Pclass + Age_filled + SibSp + Parch + Fare_filled + Embarked_filled + Child + Fare2_filled,
                    data=train,
                    importance=TRUE,
                    ntree=2000)

varImpPlot(fit)

Prediction <- predict(fit,test)
submit <- data.frame(PassengerId = test$PassengerId, Survived=Prediction)

write.csv(submit,file="working-randomforest.csv", row.names=FALSE)
