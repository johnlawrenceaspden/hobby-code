#!/usr/bin/r


## http://trevorstephens.com/kaggle-titanic-tutorial/r-part-4-feature-engineering/
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

train <- read.csv("train.csv")
test <- read.csv("test.csv")
test$Survived <- NA
combi<-rbind(train,test)

# de-factorize the names 
combi$Name <- as.character(combi$Name)


######################################################################
## Feature Engineering
######################################################################


# Pull out the title parts of the names for a separate variable
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)


## combine Madame and Mademoiselle to Mlle 
combi$Title[combi$Title %in% c('Mme')] <- 'Mrs'
combi$Title[combi$Title %in% c('Mlle')] <- 'Miss'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Jonkheer')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'



## Create FamilyID from surname and family size, all less than two are 'Small'
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'


## Here we kill off the families with 4 members who only have 2 members, etc.
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'

## Back to Factor
combi$Title <- factor(combi$Title)
combi$FamilyID <- factor(combi$FamilyID)



######################################################################
## Filling in Missing Data
######################################################################

## Some data imputation that's necessary for the standard Random Forest algorithm
## But not for the cforest that we're actually using
## removing this section drops us to 0.80861, not sure why

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),],
                method="anova")

fancyRpartPlot(Agefit)

combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])


which(combi$Embarked=='')
combi$Embarked[c(62,830)] = 'S'

combi$Embarked <- factor(combi$Embarked)

combi$Fare[1044] <- median(combi$Fare,na.rm=TRUE)




######################################################################
## Split Data Into Test and Training Sets
######################################################################

write.csv(combi,file="combi.csv", row.names=FALSE)

train <- combi[1:891,]
test <- combi[892:1309,]


##install.packages('party')
library(party)


set.seed(415)

cat("FITTING (takes 1 minute 50 seconds)\n")
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data=train,
               controls=cforest_unbiased(ntree=2000, mtry=3))

cat("PREDICTING\n")
Prediction <- predict(fit,test, OOB=TRUE,type="response")
submit <- data.frame(PassengerId = test$PassengerId, Survived=Prediction)

write.csv(submit,file="ciforest.csv", row.names=FALSE)

# 0.81340, or 340 out of 418
0.81340*nrow(test)





