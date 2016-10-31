#!/usr/bin/r

library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(party)

# Read in the test and train files, and combine them into combi so that
# feature engineering can be done identically on both sets of data
train <- read.csv("train.csv")
test <- read.csv("test.csv")
test$Survived <- NA
full<-rbind(train,test)

# de-factorize the names 
full$Name <- as.character(full$Name)


######################################################################
## Feature Engineering
######################################################################


# Pull out the title parts of the names for a separate variable
full$Title <- sapply(full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
full$Title <- sub(' ', '', full$Title)


# Various rare and foreign titles seem like the should get aggregated
full$Title[full$Title %in% c('Mme')] <- 'Mrs'
full$Title[full$Title %in% c('Mlle')] <- 'Miss'
full$Title[full$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Jonkheer')] <- 'Sir'
full$Title[full$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'



## Create FamilyID from surname and family size, all less than two are 'Small'
full$FamilySize <- full$SibSp + full$Parch + 1
full$Surname <- sapply(full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
full$FamilyID <- paste(as.character(full$FamilySize), full$Surname, sep="")
full$FamilyID[full$FamilySize <= 2] <- 'Small'


## Here we kill off the families with 4 members who only have 2 members, etc.
famIDs <- data.frame(table(full$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
full$FamilyID[full$FamilyID %in% famIDs$Var1] <- 'Small'

## Back to Factor
full$Title <- factor(full$Title)
full$FamilyID <- factor(full$FamilyID)
full$Pclass <- factor(full$Pclass)


######################################################################
## Filling in Missing Data
######################################################################

## Some data imputation that's necessary for the standard Random Forest algorithm
## But not for the cforest that we're actually using
## removing this section drops us to 0.80861, not sure why

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=full[!is.na(full$Age),],
                method="anova")

fancyRpartPlot(Agefit)

full$Age_fitted <- full$Age
full$Age_fitted[is.na(full$Age)] <- predict(Agefit, full[is.na(full$Age),])


which(full$Embarked=='')
full$Embarked[c(62,830)] = 'S'

full$Embarked <- factor(full$Embarked)

full$Fare[1044] <- median(full$Fare,na.rm=TRUE)




######################################################################
## Split Data Into Test and Training Sets
######################################################################

write.csv(full,file="full.csv", row.names=FALSE)

train <- full[1:891,]
test <- full[892:1309,]


######################################################################
## Fit model and make prediction
######################################################################


## set.seed(415)

## cat("FITTING (takes 1 minute 50 seconds)\n")
## fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
##                data=train,
##                controls=cforest_unbiased(ntree=2000, mtry=3))

## cat("PREDICTING\n")
## Prediction <- predict(fit,test, OOB=TRUE,type="response")
## submit <- data.frame(PassengerId = test$PassengerId, Survived=Prediction)

## write.csv(submit,file="ciforest.csv", row.names=FALSE)

## # 0.81340, or 340 out of 418
## 0.81340*nrow(test)




