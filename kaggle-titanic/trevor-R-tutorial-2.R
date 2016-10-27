#!/usr/bin/r


## http://trevorstephens.com/kaggle-titanic-tutorial/r-part-4-feature-engineering/
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

train <- read.csv("train.csv")
test <- read.csv("test.csv")


# Create the missing Survived column in the test data
test$Survived <- NA

# Combine the two sets into one set so we can do the same processing on each
# and so that we get factors with the same levels in both sets
combi<-rbind(train,test)

# de-factorize the names 
combi$Name <- as.character(combi$Name)

strsplit(combi$Name[1], split='[,.]')[[1]][2]

# Pull out the title parts of the names for a separate variable
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)


## combine Madame and Mademoiselle to Mlle 
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'




combi$FamilySize <- combi$SibSp + combi$Parch + 1

combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

table(combi$Surname)

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

table(combi$FamilyID)

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

table(combi$FamilyID)

## Here we kill off the families with 4 members who only have 2 members, etc.
## I think this is a terrible mistake. The others are in the private data kept secret by Kaggle, probably.
famIDs <- data.frame(table(combi$FamilyID))

table(famIDs)

famIDs <- famIDs[famIDs$Freq <= 2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'

table(combi$FamilyID)

## Back to Factor
combi$Title <- factor(combi$Title)
combi$FamilyID <- factor(combi$FamilyID)




## Some data imputation that's necessary for the Random Forest algorithm

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),],
                method="anova")

fancyRpartPlot(Agefit)

combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])


which(combi$Embarked=='')
combi$Embarked[c(62,830)] = 'S'

combi$Embarked <- factor(combi$Embarked)

combi$Fare[1044] <- median(combi$Fare,na.rm=TRUE)







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





