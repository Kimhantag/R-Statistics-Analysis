##### Chapter 6 {caret} package ; total package

## 6.2
library(caret)
library(mlbench)
data(Sonar)
set.seed(107)
inTrain <- createDataPartition(y = Sonar$Class, 
           p = .75, list = FALSE) # class M & R
str(inTrain)
training <- Sonar[ inTrain,]
testing <- Sonar[-inTrain,]
nrow(training)
nrow(testing)

plsFit <- train(Class ~ .,
          data = training,
          method = "pls", # method
          preProc = c("center", "scale")) # pre-processing

ctrl <- trainControl(method = "repeatedcv", repeats = 3)
  # CV repeat 3 times
plsFit <- train(Class ~ ., data = training,
            method = "pls",
            tuneLength = 15, #  integer (1, 15)
            trControl = ctrl, # special value
            preProc = c("center", "scale"))

set.seed(123)
ctrl <- trainControl(method = "repeatedcv", repeats = 3,
                     classProbs = TRUE, # calculate class prob.
                     summaryFunction = twoClassSummary) 
  # evaluation performance for 2 classes

plsFit <- train(Class ~ ., data = training,
            method = "pls",
            tuneLength = 15,
            trControl = ctrl,
            metric = "ROC", 
            # choose tune parametor based on ROC("accuracy", "Kappa", "RMSE", "Rsquared")
            preProc = c("center", "scale"))
plsFit
plot(plsFit)

plsClasses <- predict(plsFit, newdata = testing)
str(plsClasses)
plsProbs <- predict(plsFit, newdata = testing, type = "prob")
head(plsProbs)
confusionMatrix(data = plsClasses, testing$Class)

## rds, grid
rdaGrid = data.frame(gamma = (0:4)/4, lambda = 3/4)
set.seed(123)
rdaFit <- train(Class ~ ., data = training,
            method = "rda",
            tuneGrid = rdaGrid, 
            trControl = ctrl,
            metric = "ROC")
rdaFit
rdaClasses <- predict(rdaFit, newdata = testing)
confusionMatrix(rdaClasses, testing$Class)


## reamples many times
resamps <- resamples(list(pls = plsFit, rda = rdaFit))
summary(resamps)

xyplot(resamps, what = "BlandAltman")
  # paired t-test

diffs <- diff(resamps)
summary(diffs)


## 6.4.1 variable importance
## Exmaple 1; varImp{caret}

set.seed(100)
library(mlbench)
library(caret)
data(PimaIndiansDiabetes)

control <- trainControl(method="repeatedcv", number=10, repeats=3)
  # 10 fold CV * 3 times
model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq",
               preProcess="scale", trControl=control)
importance <- varImp(model, scale=FALSE) 
  # scale= make score (0,100)
print(importance)
plot(importance)


## 6.4.2 variable selection
## Exmaple 2; rfe{caret}
set.seed(50)
library(mlbench)
library(caret)
data(PimaIndiansDiabetes)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
  # functions=rfFuncs, lmFuncs, nbFuncs, treebagFuncs
  # method= cv, repeatedcv, boot, LOOCV, LGOCV
results <- rfe(PimaIndiansDiabetes[,1:8], PimaIndiansDiabetes[,9],
               sizes=c(1:8), rfeControl=control)
  # sizes=consider # of var
print(results)
predictors(results)
plot(results, type=c("g", "o"))
