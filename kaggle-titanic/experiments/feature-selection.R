#!/usr/bin/r

#http://machinelearningmastery.com/feature-selection-with-the-caret-r-package/
    
set.seed(7)

# install.packages("mlbench")
# install.packages("caret")
# install.packages("pROC")

library(mlbench)
library(caret)

data(PimaIndiansDiabetes)

# look for correlations
correlationMatrix <- cor(PimaIndiansDiabetes[,1:8])
print(correlationMatrix)
highlyCorrelated<-findCorrelation(correlationMatrix, cutoff=0.5)
print(highlyCorrelated)

# use caret to display feature importance
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale", trControl=control)
importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance)

# automatic feature selection
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(PimaIndiansDiabetes[,1:8], PimaIndiansDiabetes[,9], sizes=c(1:8), rfeControl=control)

print(results)
predictors(results)
plot(results,type=c("g","o"))
