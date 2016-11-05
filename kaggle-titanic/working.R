#!/usr/bin/r

library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm
library('rpart')
library('rattle')
library('rpart.plot')
library('RColorBrewer')
library('party')
library('caret')

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




table(full$Sex,full$Title)

full$BinnedTitle<-full$Title
# Various rare and foreign titles seem like they should get aggregated
full$BinnedTitle[full$Title %in% c('Mme')] <- 'Mrs'
full$BinnedTitle[full$Title %in% c('Mlle')] <- 'Miss'
full$BinnedTitle[full$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Jonkheer','Col')] <- 'Sir'
full$BinnedTitle[full$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'
table(full$Sex,full$BinnedTitle)


# Also Surnames
full$Surname <- sapply(full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})



# Women and children first!

# Now we'd like to work out who the children were
# I think the Edwardians might not have considered a 16-year old male a 'child'.
full$Child <- "Adult"
full$Child[full$Age<12 & !is.na(full$Age)] <- "Child"
full$Child[full$Title=='Master']<- "Child"

# Survival rates for women and children don't seem that different
full$WOC <- "AdultMale"
full$WOC[full$Child=="Child"] <- "WOC"
full$WOC[full$Sex=="female"] <- "WOC"

# I suspect that if we don't know your age then that's bad, because of how the records were compiled
full$AgeKnown <- "Yes"
full$AgeKnown[is.na(full$Age)]<-"No"

# Similarly babies with fractional ages seem to have survived uniformly, which must be a recording effect
full$FractionalBaby<-"No"
full$FractionalBaby[full$Age<1 & !is.na(full$Age)]<-"Yes"


## Try to identify families

## Can't use this, for two parents and four children everyone ends up 6
## but for grandfather, father, son, daughter  get 2,4,3,3
full$NoOfRelatives <- full$SibSp + full$Parch + 1

## I think it's best to identify family groupings by ticket
## Although sometimes families buy two or three consecutive tickets
## So we could combine consecutive tickets where there's evidence that something's up?
full$FamilyID <- full$Ticket

gbt=group_by(full,FamilyID)

summarize(gbt, count=n(), survived=sum(Survived==1), died=sum(Survived==0), unknown=sum(is.na(Survived)))

group_summary<-data.frame(summarize(gbt,
                         count=n(),
                         surv=sum(!is.na(Survived) & Survived==1),
                         died=sum(!is.na(Survived) & Survived==0),
                         woc=sum(WOC=="WOC"),
                         adm=sum(WOC=="AdultMale"),
                         wocsurv=sum(!is.na(Survived) & Survived==1 & WOC=="WOC"),
                         wocdied=sum(!is.na(Survived) & Survived==0 & WOC=="WOC"),
                         admsurv=sum(!is.na(Survived) & Survived==1 & WOC=="AdultMale"),
                         admdied=sum(!is.na(Survived) & Survived==0 & WOC=="AdultMale"),
                         ratio=(surv+1)/(surv+died+2),
                         admratio=(admsurv+1)/(admsurv+admdied+2),
                         wocratio=(wocsurv+1)/(wocsurv+wocdied+2),
                         unknown=sum(is.na(Survived)),
                         total=wocsurv+wocdied+admsurv+admdied+unknown,
                         disc=total-count))

full$count<-NA
full$woc<-NA
full$adm<-NA
for (i in 1:nrow(group_summary)){
    full[full$FamilyID==group_summary[i,c("FamilyID")],]$count<-group_summary[i,c("count")]
    full[full$FamilyID==group_summary[i,c("FamilyID")],]$woc<-group_summary[i,c("woc")]
    full[full$FamilyID==group_summary[i,c("FamilyID")],]$adm<-group_summary[i,c("adm")]
    }



large_group_summary <- filter(group_summary, count>3)

plot(large_group_summary$admratio,large_group_summary$wocratio)


full$ratio<-NA
full$admratio<-NA
full$wocratio<-NA
full$count<-NA
for (i in 1:nrow(large_group_summary)){
    full[full$FamilyID==group_summary[i,c("FamilyID")],]$ratio<-group_summary[i,c("ratio")]
    full[full$FamilyID==group_summary[i,c("FamilyID")],]$wocratio<-group_summary[i,c("wocratio")]
    full[full$FamilyID==group_summary[i,c("FamilyID")],]$admratio<-group_summary[i,c("admratio")]
    }

full$partytype<-paste(full$adm,full$woc,sep="-")
table(full$partytype)

table(full$partytype,full$Survived)

full$femalesinparty<-(full$woc>0)

# notice the resourceful chinese, who despite being a group of 8 males with no women in third class
# escaped by hiding in the bottom of a collapsible lifeboat!

################################################################################################

full$Sex<-factor(full$Sex)
full$Child<-factor(full$Child)
full$Survived<-factor(full$Survived)
full$partytype<-factor(full$partytype)

training <- full[1:891,]
testing <- full[892:1309,]

rm(rpartmodel)
rm(cm)
rm(pred)
rpartmodel = train( Survived ~ count+WOC+Pclass+femalesinparty+Embarked+BinnedTitle+AgeKnown+FractionalBaby, method="rpart", data=training)
rpartmodel
fancyRpartPlot(rpartmodel$finalModel)
rpartmodel$finalModel
pred=predict(rpartmodel,training)
cm<-table(pred,training$Survived)
cm
(cm[1,1]+cm[2,2])/sum(cm)
names(full)

ggplot(rpartmodel)

##################### To Submit One
Prediction=predict(rpartmodel,testing)
submit <- data.frame(PassengerId = test$PassengerId, Survived=Prediction)
write.csv(submit,file="caret.csv", row.names=FALSE)


rm(rfmodel)
rm(cm)
rm(pred)
rfmodel = train( Survived ~ count+WOC+Pclass+femalesinparty+Embarked+BinnedTitle+AgeKnown+FractionalBaby, method="rf", data=training)
rfmodel

pred=predict(rfmodel,training)
cm<-table(pred,training$Survived)
cm
(cm[1,1]+cm[2,2])/sum(cm)

names(full)
ggplot(rfmodel)

##################### To Submit One
Prediction=predict(rfmodel,testing)
submit <- data.frame(PassengerId = test$PassengerId, Survived=Prediction)
write.csv(submit,file="caret.csv", row.names=FALSE)





## 0.78 on just Title
## 0.78 on Just Sex
## 0.78 on Sex and Title
## 
rpartmodel = train( Survived ~ Pclass+Sex+Embarked+BinnedTitle+Child+WOC+AgeKnown+FractionalBaby, method="rpart", data=training)
rpartmodel
fancyRpartPlot(rpartmodel$finalModel)
rpartmodel$finalModel
names(full)

ggplot(rpartmodel)


## This does the same thing with rpart directly, but you don't get cross validation accuracy
## Or at least it's wrong. What is xval?
rpm<-rpart(Survived ~ Sex + Child, data=training, method="class")
rpm
printcp(rpm)
fancyRpartPlot(rpm)




##############################################################################################







famIDs <- famIDs[famIDs$Freq <= 2,]
full$FamilyID[full$FamilyID %in% famIDs$Var1] <- 'Small'

## Let's also make a group identifier out of the ticket numbers
full$Ticket <- as.character(full$Ticket)
table(full$Ticket)

groupIDs <- data.frame(table(full$Ticket))

table(groupIDs$Freq)

group1IDs <- groupIDs[groupIDs$Freq == 1,]
group2IDs <- groupIDs[groupIDs$Freq == 2,]

full$TicketGroup<-full$Ticket
full$TicketGroup[full$Ticket %in% group1IDs$Var1] <- 'Singleton'
full$TicketGroup[full$Ticket %in% group2IDs$Var1] <- 'Couple'


mosaicplot(table(full$TicketGroup, full$Survived), main='Ticket Group by Survival', shade=TRUE)

couples<-full[full$TicketGroup=='Couple',]
mosaicplot(table(couples$Sex,couples$Survived))

singles<-full[full$TicketGroup=='Singleton',]
mosaicplot(table(singles$Sex,singles$Survived))





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



## Children are those under 10
full$Child <- 0
full$Child[full$Age < 10]<-1

## Bin the fares
full$Fare2 <- '30+'
full$Fare2[full$Fare < 30 & full$Fare >= 20] <- '20-30'
full$Fare2[full$Fare < 20 & full$Fare >= 10] <- '10-20'
full$Fare2[full$Fare < 10] <- '<10'

## Back to Factor
full$TicketGroup <- factor(full$TicketGroup)
full$Title <- factor(full$Title)
full$FamilyID <- factor(full$FamilyID)
full$Pclass <- factor(full$Pclass)
full$Survived <- factor(full$Survived)

######################################################################
## Data Plots
######################################################################


## Survival by Family Size
ggplot(full[1:891,], aes(x = FamilySize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()


######################################################################
## Examine Processed Data
######################################################################

famIDs <- data.frame(table(full$FamilyID))

famIDs[with(famIDs, order(Freq)),]

bigfams=famIDs[famIDs$Freq>4,1]

justthebigfams=full[full$FamilyID %in% bigfams,]

mosaicplot(table(justthebigfams$FamilyID, justthebigfams$Survived))

table(justthebigfams$FamilyID, justthebigfams$Survived)

for (i in seq(length(bigfams))) {
    a=full[full$FamilyID == as.character(bigfams[i]),]
    print (a)
    print(table(a$Sex))
    print(table(a$Survived,a$Sex))
}





## The Vander Plankes
filter(full, Surname=='Vander Planke')
## Appear to have been three siblings, and one wife, travelling on two separate tickets.
## This should likely be one family group

## The Richards appear inexplicable until you realise that 
filter(full,Surname=='Richards')
## they and the hockings are all one family (Except for Mr SJM Hocking, who's not related to them)
filter(full,Surname=='Hocking')
## So this family has ticket numbers 29104,29105,29106

## The Lahtinens also look difficult
filter(full,Surname=='Lahtinen')
## But they are travelling with a sister Lylli Silven, who seems to
## have been recorded as their daughter in this dataset
filter(full,Ticket=='250652' | Ticket=='250651')

## Hmm, looks like there are 7 Anderssons, parents and five children, on ticket 347082
## They all died, but there are other Andersons all on separate tickets, who did much better
## and yet are somehow also recorded as having 4 siblings and 2 parents, which
## looks like a mistake in the data
full[full$Surname=='Andersson',c("Sex","SibSp","Parch","Ticket","FamilyID","Survived","Pclass")]
## Notice that the doomed family have paid a high fare, because there were seven of them
full[full$Surname=='Andersson' & full$Ticket=="347082",c("Sex","SibSp","Parch","Ticket","FamilyID","Survived","Pclass","Age","Fare")]
## Other Anderssons did much better, despite being in third class
full[full$Surname=='Andersson' & full$Ticket!="347082",c("Sex","SibSp","Parch","Ticket","FamilyID","Survived","Pclass","Age","Fare")]
## Are they all singletons (did single women travel in 3rd class on the titanic?)
## Or are they two couples, in which case why have they got different tickets?

# How does the sex-related survival ratio behave with age
table(full$Age<12, full$Survived, full$Sex)
prop.table(table(full$Age<12, full$Survived, full$Sex),c(1,3))
(table(full$Age<12, full$Survived, full$Sex))
mosaicplot(table(full$Age<12, full$Survived, full$Sex))


## Have a look at the processed data frame
str(full)

## Amelia has a nice function to visualize missing values
require(Amelia)
missmap(full)

## Age vs Title boxplot
boxplot(train$Age ~ train$Title)

mosaicplot(train$Title ~ train$Survived, shade=F, color=T)

######################################################################
## Split Data Into Test and Training Sets
######################################################################

write.csv(full,file="full.csv", row.names=FALSE)

train <- full[1:891,]
test <- full[892:1309,]

######################################################################
## Fit model and make prediction
######################################################################

fit <- rpart(Survived ~ Sex + Pclass + Age + SibSp + Parch + Fare + Embarked + Child + Fare2 + Title, data=train, method="class")
fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived=Prediction)
write.csv(submit, file = "decisiontree.csv", row.names = FALSE)



######################################################################
## Attempt to use caret
######################################################################

library(caret)

## 0.78 on just Title
## 0.78 on Just Sex
## 0.78 on Sex and Title
rpartmodel = train( factor(Survived) ~ Sex + Fare2 + Pclass, method="rpart", data=train, control=fitControl)
rpartmodel
fancyRpartPlot(rpartmodel$finalModel)
predict(rpartmodel,test)
ggplot(rpartmodel)





rpartmodel = train( factor(Survived) ~ Sex + Pclass + Child + Fare2 + Title, method="rpart", data=train)
rpartmodel
fancyRpartPlot(rpartmodel$finalModel)
predict(rpartmodel,test)



ctree_model = train( factor(Survived) ~ Sex + Pclass + Child + Fare2 + Title, method="ctree", data=train)
ctree_model
plot(ctree_model$finalModel)
predict(ctree_model,test)


cforest_model = train( factor(Survived) ~ Sex + Pclass + Child + Fare2 + Title, method="cforest", data=train)
cforest_model


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





