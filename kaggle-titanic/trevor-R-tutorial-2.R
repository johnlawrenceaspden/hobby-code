#!/usr/bin/r

## http://trevorstephens.com/kaggle-titanic-tutorial/r-part-4-feature-engineering/
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

train <- read.csv("train.csv")
test <- read.csv("test.csv")

train$Name[1]
test$Name[1]

# Create the missing Survived column in the test data
test$Survived <- NA

# Combine the two sets into one set so we can do the same processing on each
# and so that we get factors with the same levels in both sets
combi<-rbind(train,test)

# de-factorize the names 
combi$Name <- as.character(combi$Name)

combi$Name[1]

strsplit(combi$Name[1], split='[,.]')[[1]][2]

# Pull out the title parts of the names for a separate variable
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)

table(combi$Title)
table(combi$Title,combi$Age)

## combine Madame and Mademoiselle to Mlle 
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'

## Male nobility to Sir
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'

## Female Nobility to Lady
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

## Hmm, Don and Dona are Mr and Mrs or Sir and Lady?, and Jonkheer is a Dutch male noble,Mme and Mlle are Mrs and Miss, Ms is ?
table(combi$Title)

## Back to Factor
combi$Title <- factor(combi$Title)


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

# Scores 0.79426, or 332 correct predictions
0.79426*nrow(test)


## http://trevorstephens.com/kaggle-titanic-tutorial/r-part-5-random-forests/

# random sampling with replacement
sample(1:10, replace = TRUE)

summary(combi$Age)

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),],
                method="anova")

fancyRpartPlot(Agefit)

combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

summary(combi$Age)
hist(combi$Age)

summary(combi)

summary(combi$Embarked)

which(combi$Embarked=='')
combi$Embarked[c(62,830)] = 'S'

summary(combi$Embarked)
str(combi$Embarked)

combi$Embarked <- factor(combi$Embarked)

summary(combi$Embarked)
str(combi$Embarked)

summary(combi$Fare)

which(is.na(combi$Fare))

combi$Fare[1044] <- median(combi$Fare,na.rm=TRUE)

## reduce family factor since random forests can't deal with more than 32 levels

combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

str(combi$FamilyID2)

## install.packages('randomForest')
## sudo apt install r-cran-randomforest
library(randomForest)

train <- combi[1:891,]
test <- combi[892:1309,]


set.seed(415)

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2,
                    data=train,
                    importance=TRUE,
                    ntree=2000)
                   

varImpPlot(fit)

Prediction <- predict(fit,test)
submit <- data.frame(PassengerId = test$PassengerId, Survived=Prediction)

write.csv(submit,file="firstforest.csv", row.names=FALSE)



