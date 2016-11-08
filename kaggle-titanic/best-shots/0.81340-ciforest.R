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

## Construct a Family Identifier
combi$FamilySize <- combi$SibSp + combi$Parch + 1


#####################################################################
# Family Identifier
#####################################################################

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

table(combi$FamilyID)

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

combi$FamilyID <- factor(combi$FamilyID)

table(combi$FamilyID)

## Here we kill off the families with 4 members who only have 2 members, etc.
## This is caused by the weird behaviour of the SibSp/Parch things with respect to
## extended families
famIDs <- data.frame(table(combi$FamilyID))

table(famIDs)

famIDs <- famIDs[famIDs$Freq <= 2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'

table(combi$FamilyID)


#####################################################################
# Imputation of missing data
#####################################################################




Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),],
                method="anova")

combi$Age2<-combi$Age
combi$Age2[is.na(combi$Age2)] <- predict(Agefit, combi[is.na(combi$Age),])

# The two missing embarkees probably got on at Cherbourg, for reasons explained in the Megan tutorial
combi$Embarked[c(62,830)] = 'C'

combi$Embarked <- factor(combi$Embarked)

combi$Fare[1044] <- median(combi$Fare,na.rm=TRUE)


######################################################################
## Fit Model and Make Predictions
######################################################################

training <- combi[1:891,]
testing <- combi[892:1309,]




start <- Sys.time ()
print(start)
cat("FITTING CIFOREST (takes ~ 4 minutes 20 seconds)\n")
set.seed(415) # changing random seed to 1 makes more survival predictions, slightly lowers confusion matrix accuracy, makes no difference in Kaggle submission

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age2 + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data=training,
               controls=cforest_unbiased(ntree=2000, mtry=3))
cat("FIT COMPLETE\n")
end=Sys.time () - start
print(end)

# create confusion matrix (there is a subtle bug here, see https://github.com/topepo/caret/issues/351)
cat("Calculating Confusion Matrix\n")
trainingPrediction <- predict(fit, OOB=TRUE) #predict(fit,training ,OOB=TRUE) ignores OOB=TRUE!

confusionmatrix<-table(training$Survived,trainingPrediction)
print(confusionmatrix)
print((confusionmatrix[1,1]+confusionmatrix[2,2])/sum(confusionmatrix))

# we're overpredicting death

## trainingPrediction
##       0   1
##   0 497  52
##   1 103 239

(497+239)/(497+52+103+239)
#0.8260382 isn't too far away from the actual result on the kaggle submission, which is 0.81340

# make real predictions
cat("Predicting on Test Data\n")
Prediction <- predict(fit,testing, OOB=TRUE) #need to put OOB here, but what is it doing?

# write data
submit <- data.frame(PassengerId = testing$PassengerId, Survived=Prediction)
write.csv(submit,file="0.81340-ciforest.csv", row.names=FALSE)

# 0.81340, or 340 out of 418
0.81340*nrow(testing)




