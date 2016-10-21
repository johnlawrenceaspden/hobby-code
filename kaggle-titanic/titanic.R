## install required R packages
## sudo apt install r-cran-ggplot2 r-cran-scales r-cran-dplyr r-cran-randomforest
## turn off infuriating underscore thing in ESS
## (ess-toggle-underscore nil)

library('ggplot2')
library('ggthemes')
library('scales')
library('dplyr')
library('mice')
library('randomForest')


train <- read.csv('train.csv')
test  <- read.csv('test.csv')

full <- bind_rows(train, test)

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

ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived)))+geom_bar(stat='count', position='dodge')+scale_x_continuous(breaks=c(1:11))+labs(x = 'Family Size')

## Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize >1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

## Show family size by survival using a mosaic plot
mosaicplot(table(full$FsizeD,full$Survived), main='Family Size by Survival', shade=TRUE)





