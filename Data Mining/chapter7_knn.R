##### Chapter 7 knn

## Example 1; knn{class}
library(class)
data(iris3)
train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3]) 
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25))) # y
knn(train, test, cl, k = 3, prob=TRUE)


## Example 2; kNN{DMwR}
library(DMwR)
data(iris)
idxs <- sample(1:nrow(iris), as.integer(0.7*nrow(iris)))
trainIris <- iris[idxs,]
testIris <- iris[-idxs,]
nn3 <- kNN(Species ~ ., trainIris, testIris, norm=FALSE, k=3)
table(testIris[,'Species'], nn3)


## Example 3; kknn{kknn}
library(kknn)
data(iris)
m <- dim(iris)[1]
val <- sample(1:m, size=round(m/3), replace=FALSE, prob=rep(1/m, m))
iris.learn <- iris[-val,]
iris.valid <- iris[val,]
iris.kknn <- kknn(Species~., iris.learn, iris.valid, distance=1,
                kernel="triangular")
summary(iris.kknn)
fit <- fitted(iris.kknn)
table(iris.valid$Species, fit)

pcol <- as.character(as.numeric(iris.valid$Species))
pairs(iris.valid[1:4], pch=pcol,
      col=c("green3", "red")[(iris.valid$Species != fit)+1])


## Example 4; {FNN}
full <- data.frame(name=c("McGwire,Mark", "Bonds,Barry",
                          "Helton,Todd", "Walker,Larry",
                          "Pujols,Albert", "Pedroia,Dustin"),
                 lag1=c(100,90,75,89,95,70),
                 lag2=c(120,80,95,79,92,90),
                 Runs=c(65,120,105,99,65,100))
full

library(kknn)
train <- full[full$name!="Bonds,Barry",]
test <- full[full$name=="Bonds,Barry",]
k <- kknn(Runs~lag1+lag2, train=train, test=test, k=2, distance=1)
fit <- fitted(k)
fit
names(k)
k$fitted.values
k$CL
k$W
k$C
train[c(k$C),]

library(FNN)
get.knnx(data=train[,c("lag1","lag2")], query=test[,c("lag1","lag2")], k=2)
  # find neighbor
train[c(3,4), "name"]



## 7.3 knn using {caret}
## (a) createDataPartition{caret}
library(ISLR)
library(caret)
set.seed(100)
indxTrain <- createDataPartition(y = Smarket$Direction, p = 0.75, list = FALSE)
training <- Smarket[indxTrain,]
testing <- Smarket[-indxTrain,]
prop.table(table(training$Direction)) * 100
prop.table(table(testing$Direction)) * 100
prop.table(table(Smarket$Direction)) * 100


## (b) preProcess{caret}
trainX <- training[,names(training) != "Direction"]
preProcValues <- preProcess(x = trainX, method = c("center", "scale"))
preProcValues


## (c) train{caret}
library(e1071)
set.seed(200)
ctrl <- trainControl(method="repeatedcv", repeats = 3) 
knnFit <- train(Direction ~ ., data = training, method = "knn", # knn
                trControl = ctrl, preProcess = c("center","scale"), 
                tuneLength = 20) # up to k=20
knnFit

plot(knnFit)
knnPredict <- predict(knnFit, newdata = testing )
confusionMatrix(knnPredict, testing$Direction )
mean(knnPredict == testing$Direction)

set.seed(200)
ctrl <- trainControl(method="repeatedcv", repeats = 3,
                     classProbs=TRUE, summaryFunction = twoClassSummary)
knnFit <- train(Direction ~ ., data = training, method = "knn",
                trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
knnFit

plot(knnFit, print.thres = 0.5, type="S")
knnPredict <- predict(knnFit, newdata = testing )
confusionMatrix(knnPredict, testing$Direction )
mean(knnPredict == testing$Direction)

#ROC curve
library(pROC)
knnPredict <- predict(knnFit, newdata = testing , type="prob")
knnPredict
knnROC <- roc(testing$Direction, #y
              knnPredict[,"Down"], # prob. of "Down"
              levels = levels(testing$Direction))
knnROC
plot(knnROC, type="S", print.thres= 0.5)


## (d) train{caret} method="rf"
set.seed(300)
ctrl <- trainControl(method="repeatedcv", repeats = 3)
rfFit <- train(Direction ~ ., data = training, method = "rf",
               trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
rfFit

plot(rfFit)
rfPredict <- predict(rfFit, newdata = testing )
confusionMatrix(rfPredict, testing$Direction )
mean(rfPredict == testing$Direction)

# summaryFunction=twoClassSummary
set.seed(300)
ctrl <- trainControl(method="repeatedcv", repeats = 3,
                       classProbs=TRUE, summaryFunction=twoClassSummary)
rfFit <- train(Direction ~ ., data = training, method = "rf",
                 trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
rfFit

plot(rfFit, print.thres = 0.5, type="S")
rfPredict <- predict(rfFit, newdata = testing )
confusionMatrix(rfPredict, testing$Direction )
mean(rfPredict == testing$Direction)

# ROC
library(pROC)
rfPredict <- predict(rfFit,newdata = testing , type="prob")
rfROC <- roc(testing$Direction,rfPredict[,"Down"],
             levels = rev(testing$Direction))
rfROC
plot(rfROC, type="S", print.thres= 0.5)
