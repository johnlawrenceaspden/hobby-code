#!/usr/bin/r

## http://trevorstephens.com/kaggle-titanic-tutorial/r-part-4-feature-engineering/
    
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

train <- read.csv("train.csv")
test <- read.csv("test.csv")

train$Name[1]

test$Survived <- NA

combi<-rbind(train,test)

combi$Name <- as.character(combi$Name)

combi$Name[1]

strsplit(combi$Name[1], split='[,.]')[[1]][2]

combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})

combi$Title <- sub(' ', '', combi$Title)

table(combi$Title)

## combine Madame and Mademoiselle to Mlle 
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'

## Male nobility to Sir
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'

## Female Nobility to Lady
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

## Hmm, Don and Dona are Mr and Mrs, surely, and Jonkheer is a Dutch male
table(combi$Title)

## Back to Factor
combi$Title <- factor(combi$Title)


combi$FamilySize <- combi$SibSp + combi$Parch + 1

combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

table(combi$Surname)

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

table(combi$FamilyID)

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

## Here we kill off the families with 4 members who only have 2 members, etc.
## I think this is a terrible mistake. The others are in the private data kept secret by Kaggle, probably.
famIDs <- data.frame(table(combi$FamilyID))

famIDs <- famIDs[famIDs$Freq <= 2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'

## It's crucial, I think, to turn this into a factor before splitting, so that the model can
## be trained properly
combi$FamilyID <- factor(combi$FamilyID)

train <- combi[1:891,]
test <- combi[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data=train, 
               method="class")

fancyRpartPlot(fit)


Prediction <- predict(fit, test, type = "class")


submit <- data.frame(PassengerId = test$PassengerId, Survived=Prediction)
write.csv(submit, file = "featureengineeringanddecisiontree.csv", row.names = FALSE)
