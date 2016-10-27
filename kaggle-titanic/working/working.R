#!/usr/bin/r

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(party)

train <- read.csv("../train.csv", stringsAsFactors=FALSE)
test <- read.csv("../test.csv", stringsAsFactors=FALSE)
test$Survived <- NA
combi<-rbind(train,test)

######################################################################
## Feature Engineering
######################################################################

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

## Get the Surnames out of the Names
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

famIDs <- data.frame(table(combi$FamilyID))

famIDs <- famIDs[famIDs$Freq <= 2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'

######################################################################
## Missing Data Fiddling
######################################################################


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

######################################################################
## Factorize and Split the Data Into Test and Training Sets
######################################################################



combi$Pclass=factor(combi$Pclass)
combi$Sex=factor(combi$Sex)
combi$Embarked=factor(combi$Embarked)
combi$Embarked_filled=factor(combi$Embarked_filled)
combi$Child <- as.factor(combi$Child)
combi$Fare2 <- as.factor(combi$Fare2)
combi$Fare2_filled <- as.factor(combi$Fare2_filled)
combi$Title <- factor(combi$Title)
combi$Ticket <- factor(combi$Ticket)
combi$FamilyID <- factor(combi$FamilyID)

train <- combi[1:891,]
test <- combi[892:1309,]



######################################################################
## Model Fitting and Prediction
######################################################################



## Fit a Decision Tree using rpart
## adding Family Size actually hurts us
## adding Ticket destroys it!
## Filling in missing Age values hurts us
tree_fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Child + Fare2 + Title + FamilySize + FamilyID, data=train, method="class")
fancyRpartPlot(tree_fit)

tree_Prediction <- predict(tree_fit, test, type = "class")
tree_submit <- data.frame(PassengerId = test$PassengerId, Survived=tree_Prediction)
write.csv(tree_submit, file = "working-rpart.csv", row.names = FALSE)

## scores 0.80303 335.6665? out of 418
0.80303*nrow(test)

## Fit a Random Forest using randomForest
set.seed(415)
forest_fit <- randomForest(as.factor(Survived) ~ Sex + Pclass + Age_filled + SibSp + Parch + Fare_filled + Embarked_filled + Child + Fare2_filled + Title + FamilyID,
                    data=train,
                    importance=TRUE,
                    ntree=2000)

varImpPlot(forest_fit)

forest_Prediction <- predict(forest_fit,test)
forest_submit <- data.frame(PassengerId = test$PassengerId, Survived=forest_Prediction)
write.csv(forest_submit,file="working-randomforest.csv", row.names=FALSE)

## scores 0.77990, 326 out of 418

## Trevor's best prediction was with party's cforests, using set_seed(415) and 
## as.factor(Survived) ~ Pclass + Sex + Age_filled + SibSp + Parch + Fare_filled + Embarked_filled + Title + FamilySize + FamilyID

## Trevor got 81340 but I get 80861 with this code

## Fit a forest of conditional inference trees using party
set.seed(415)
cforest_fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age_filled + SibSp + Parch + Fare_filled + Embarked_filled + Title + FamilySize + FamilyID,
               data=train,
               controls=cforest_unbiased(ntree=2000, mtry=3))

cforest_Prediction <- predict(cforest_fit,test, OOB=TRUE,type="response")
cforest_submit <- data.frame(PassengerId = test$PassengerId, Survived=cforest_Prediction)
write.csv(cforest_submit,file="working-ciforest.csv", row.names=FALSE)


