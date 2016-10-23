## Megan Risdal's kaggle tutorial on the titanic dataset
## https://www.kaggle.com/mrisdal/titanic/exploring-survival-on-the-titanic/notebook

## install required R packages
## sudo apt install r-cran-ggplot2 r-cran-scales r-cran-dplyr r-cran-randomforest
## mice and VIM and ggthemes need to be installed by hand
## > install.packages('ggthemes', dep = TRUE)
## > install.packages('mice', dep=TRUE)
## > install.packages('VIM', dep=TRUE)


## unavailable errors are likely due to using a bad mirror, try the CA1 mirror

## turn off infuriating underscore thing in ESS
## (ess-toggle-underscore nil) 

library('ggplot2')
library('ggthemes')
library('scales')
library('dplyr')
library('mice')
library('randomForest')
library('VIM')


## read in training and test data
train <- read.csv('train.csv', stringsAsFactors = F)
test  <- read.csv('test.csv', stringsAsFactors = F)
train_size <- nrow(train)
test_size <- nrow(test)

## bind them and process them together, then split apart again
full <- bind_rows(train, test)
full$Sex <-factor(full$Sex)
full$Pclass <-factor(full$Pclass)
full$Survived <-factor(full$Survived)
full$Embarked <-factor(full$Embarked)
full$SibSp <-factor(full$SibSp)
full$Parch <-factor(full$Parch)
processed_train <- full[1:train_size,]
processed_test  <- full[(train_size+1):(train_size+test_size),]

## choose a random seed to make things repeatable
set.seed(754)

summary(processed_train)
rf_model <- randomForest(Survived ~ Sex + Fare + Pclass + Embarked + SibSp + Parch,  data=processed_train)
rf_model
plot(rf_model)
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
    y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
    hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()









rf_model <- randomForest(factor(Survived) ~ PassengerId + Pclass + Name + Sex + Age + SibSp + Parch + Ticket + Fare + Cabin + Embarked, data=train)

processed_train_stripped <-subset(processed_train,select=-Age:-Ticket)
md.pattern(processed_train_stripped)




full$Title <- gsub('(.*, )|(\\..*)','',full$Name)

table(full$Sex, full$Title)

rare_title <- c('Dona','Lady','the Countess','Capt','Col','Don','Dr','Major','Rev','Sir','Jonkheer')

full$Title[full$Title == 'Mlle'] <- 'Miss'
full$Title[full$Title == 'Ms'] <- 'Miss'
full$Title[full$Title == 'Mme'] <- 'Mrs'
full$Title[full$Title %in% rare_title] <- 'Rare Title'

table(full$Sex, full$Title)

full$Surname <- sapply(full$Name,function(x) strsplit(x,split='[,.]')[[1]][1])

cat(paste('We have <b>', nlevels(factor(full$Surname)),'</b> unique surnames. I would be interested to infer ethnicity based on surname --- another time.'))

## Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1

## Create a family variable
full$Family <- paste(full$Surname, full$Fsize, sep='_')

ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived)))+
    geom_bar(stat='count', position='dodge')+
    scale_x_continuous(breaks=c(1:11))+
    labs(x = 'Family Size')+
    theme_few()

## Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize >1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

## Show family size by survival using a mosaic plot
mosaicplot(table(full$FsizeD,full$Survived), main='Family Size by Survival', shade=TRUE)

full$Cabin

strsplit(full$Cabin[2],NULL)[[1]]

full$Deck <-factor(sapply(full$Cabin, function(x) strsplit(x,NULL)[[1]][1]))

# Passengers 62 and 830 are missing embarkment data
full[c(62,830),'Embarked']

cat(paste('We will infer their values for **embarkment** based on present data that we can imagine may be relevant: **passenger class** and **fare**. We see that they paid<b> $', full[c(62, 830), 'Fare'][[1]][1], '</b>and<b> $', full[c(62, 830), 'Fare'][[1]][2], '</b>respectively and their classes are<b>', full[c(62, 830), 'Pclass'][[1]][1], '</b>and<b>', full[c(62, 830), 'Pclass'][[1]][2], '</b>. So from where did they embark?'))

embark_fare <- full %>% filter(PassengerId != 62 & PassengerId != 830)

ggplot(embark_fare, aes(x=Embarked, y=Fare, fill=factor(Pclass))) +
    geom_boxplot() +
    geom_hline(aes(yintercept=80), colour='red', linetype='dashed', lwd=2)+
    scale_y_continuous(labels=dollar_format())+
    theme_few()

## Since their fare was $80 for 1st class, they most likely embarked from 'C'

full$Embarked[c(62,830)] <- 'C'







######################################################
n = c(2, 3, NA) 
s = c("aa", "bb", "cc") 
b = c(TRUE, FALSE, TRUE) 
df = data.frame(n, s, b)       # df is a data frame

