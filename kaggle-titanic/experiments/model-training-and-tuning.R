#!/usr/bin/r

# Model Training and Tuning with the CARET library
# http://topepo.github.io/caret/model-training-and-tuning.html


library(mlbench)
data(Sonar)

help(Sonar)

str(Sonar[,1:10])

library(caret)

set.seed(998)

inTraining <- createDataPartition(Sonar$Class, p=.75, list=FALSE)

training <- Sonar[inTraining,]
testing    <- Sonar[-inTraining,]

prop.table(table(training$Class))
prop.table(table(testing$Class))
prop.table(table(Sonar$Class))

# 10-fold cross validation repeated 10 times
fitControl <- trainControl(method = "repeatedcv", number=10, repeats=10)

set.seed(825)

# The verbose=FALSE option is not for caret, it's for gbm and passes through
gbmFit1 <- train(Class ~ ., data=training, method="gbm", trControl=fitControl, verbose=FALSE)

gbmFit1

plot(gbmFit1)
plot(gbmFit1, metric="Kappa")
ggplot(gbmFit1)

confusionMatrix(gbmFit1)

classpredictions=predict(gbmFit1, newdata=testing)
probabilitypredictions=predict(gbmFit1, newdata=testing, type="prob")

table(testing$Class==classpredictions)




## Ooh look there's some titanic data
## install.packages("earth")

library(earth)
data(etitanic)

head(model.matrix(survived ~ ., data=etitanic))










