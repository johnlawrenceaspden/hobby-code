#!/usr/bin/r

library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(party)


train <- read.csv("../train.csv")
test <- read.csv("../test.csv")

test$Survived <- NA
combi<-rbind(train,test)

# de-factorize the names 
combi$Name <- as.character(combi$Name)

# Pull out the title parts of the names for a separate variable
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)

# Surnames too
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})


# Merge Rare titles into closest equivalent
combi$Title[combi$Title %in% c('Mme')] <- 'Mrs'
combi$Title[combi$Title %in% c('Mlle')] <- 'Miss'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Jonkheer')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'

## Back to Factor
combi$Title <- factor(combi$Title)


combi$FamilySize <- combi$SibSp + combi$Parch + 1


table(combi$Surname)

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

table(combi$FamilyID)

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

combi$FamilyID <- factor(combi$FamilyID)

table(combi$FamilyID)

## Here we kill off the families with 4 members who only have 2 members, etc.
## I think this is a terrible mistake. The others are in the private data kept secret by Kaggle, probably.
famIDs <- data.frame(table(combi$FamilyID))

table(famIDs)

famIDs <- famIDs[famIDs$Freq <= 2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'

table(combi$FamilyID)


Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),],
                method="anova")

combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])


combi$Embarked[c(62,830)] = 'S'

combi$Embarked <- factor(combi$Embarked)

combi$Fare[1044] <- median(combi$Fare,na.rm=TRUE)





train <- combi[1:891,]
test <- combi[892:1309,]




start <- Sys.time ()
print(start)
cat("FITTING CIFOREST (takes ~ 4 minutes 20 seconds)\n")
set.seed(415)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data=train,
               controls=cforest_unbiased(ntree=2000, mtry=3))
cat("FIT COMPLETE\n")
end=Sys.time () - start
print(end)

Prediction <- predict(fit,test, OOB=TRUE,type="response")
submit <- data.frame(PassengerId = test$PassengerId, Survived=Prediction)

write.csv(submit,file="0.81340-ciforest.csv", row.names=FALSE)

# 0.81340, or 340 out of 418
0.81340*nrow(test)




