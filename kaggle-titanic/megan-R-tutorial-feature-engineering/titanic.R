#!/usr/bin/r

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
train <- read.csv('../train.csv', stringsAsFactors = F)
test  <- read.csv('../test.csv', stringsAsFactors = F)

## bind them and process them together, then split apart again
full <- bind_rows(train, test)


# Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# Show title counts by sex
table(full$Sex, full$Title)


# My title improvements reduce accuracy to 0.77990, boo!
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss' 
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% c('Dona', 'Lady', 'the Countess')]  <- 'Lady'
full$Title[full$Title %in% c('Capt', 'Col', 'Don', 'Major', 'Sir', 'Jonkheer')]  <- 'Lord'
## Being a doctor doesn't seem to have made much difference
full$Title[full$Title=='Dr' & full$Sex=='female'] <- 'Mrs'
full$Title[full$Title=='Dr' & full$Sex=='male'] <- 'Mr'
## But being a 'Rev' appears to have been fatal, so leave that

mosaicplot(table(full$Title,full$Survived), main='Survival by Title', shade=TRUE)


# Show title counts by sex again
table(full$Sex, full$Title)

# Finally, grab surname from passenger name
full$Surname <- sapply(full$Name,  
                      function(x) strsplit(x, split = '[,.]')[[1]][1])


cat(paste('We have <b>', nlevels(factor(full$Surname)),'</b> unique surnames. I would be interested to infer ethnicity based on surname --- another time.'))

## Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1

## Create a family variable
full$Family <- paste(full$Surname, full$Fsize, sep='_')

## Plot of family size vs survival
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

# This variable appears to have a lot of missing values
full$Cabin[1:28]

# The first character is the deck. For example:
strsplit(full$Cabin[2], NULL)[[1]]

# Create a Deck variable. Get passenger deck A - F:
full$Deck<-(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
full$Deck[is.na(full$Deck)]<-'Unknown'
full$Deck<-as.factor(full$Deck)

mosaicplot(table(full$Deck,full$Survived), main='Side by Survival', shade=TRUE)


## Odd or even cabin numbers tell you which side of the ship you were on
full$CabinNumber<-sapply(full$Cabin, function(x) as.integer(strsplit(x, '[ABCDEFGT]')[[1]][2]))

full$Side<-'Unknown'
full$Side[full$CabinNumber%%2==0] <-'Port'
full$Side[full$CabinNumber%%2==1] <-'Starboard'

full$Side<-as.factor(full$Side)

mosaicplot(table(full$Side,full$Survived), main='Side by Survival', shade=TRUE)


mosaicplot(table(full$Embarked,full$Survived), main='Embarked by Survival', shade=TRUE)

mosaicplot(~ Deck + Survived , data = full, color = TRUE)

################################################################################
## Missingness
################################################################################

# Passengers 62 and 830 are missing Embarkment
full[c(62, 830), 'Embarked']


cat(paste('We will infer their values for **embarkment** based on present data that we can imagine may be relevant: **passenger class** and **fare**. We see that they paid<b> $', full[c(62, 830), 'Fare'][[1]][1], '</b>and<b> $', full[c(62, 830), 'Fare'][[1]][2], '</b>respectively and their classes are<b>', full[c(62, 830), 'Pclass'][[1]][1], '</b>and<b>', full[c(62, 830), 'Pclass'][[1]][2], '</b>. So from where did they embark?'))

## this bit won't run under littler, but works fine at the REPL or in EMACS
## embark_fare <- full %>% filter(PassengerId != 62 & PassengerId != 830)
## embark_fare2 <-  filter(full, PassengerId != 62 & PassengerId != 830)

## ggplot(embark_fare, aes(x=Embarked, y=Fare, fill=factor(Pclass))) +
##      geom_boxplot() +
##      geom_hline(aes(yintercept=80), colour='red', linetype='dashed', lwd=2)+
##      scale_y_continuous(labels=dollar_format())+
##      theme_few()

## Since their fare was $80 for 1st class, they most likely embarked from 'C'

full$Embarked[c(62,830)] <- 'C'

full[1044,]

ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
  aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
    colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()

full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

################################################################################
## Predictive Imputation of Ages 
################################################################################

sum(is.na(full$Age))



# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId', 'Name', 'Ticket', 'Cabin', 'Family', 'Surname', 'Survived')], method='rf')


# Save the complete output 
mice_output <- complete(mice_mod)


# Plot age distributions
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
  col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
  col='lightgreen', ylim=c(0,0.04))


# Replace Age variable from the mice model.
full$Age <- mice_output$Age

# Show new number of missing Age values
sum(is.na(full$Age))

################################################################################
## Feature Engineering 2
################################################################################


# First we'll look at the relationship between age & survival
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex) + 
  theme_few()

# Create the column child, and indicate whether child or adult
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

table(full$Child, full$Survived)

# Adding Mother variable
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'

#Show Counts
table(full$Mother, full$Survived)

# Finish by factorizing our two new factor variables
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)


md.pattern(full)

# Split the data back into a train set and a test set
train <- full[1:891,]
test <- full[892:1309,]

# Set a random seed
set.seed(754)

# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                                            Fare + Embarked + Title + 
                                            FsizeD + Child + Mother + Side + Deck,
                                            data = train)

# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)


# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))



# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))


# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
    y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
    hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()





# Predict using the test set
prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)

# My various improvements have so broken this that it now scores 0.77512, for 324 out of 418 predictions, down from 0.80383 in its undamaged form
nrow(test)*0.77512



