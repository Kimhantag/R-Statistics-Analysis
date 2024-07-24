##### Chapter 10 ENSEMBLE MODEL 

## Example 1; bagging{adabag}
library(adabag)
data(iris)
iris.bagging <- bagging(Species~., data=iris, 
                        mfinal=10) # mfinal; repeat times
iris.bagging$importance

plot(iris.bagging$trees[[10]])
text(iris.bagging$trees[[10]])

pred <- predict(iris.bagging, newdata=iris)
table(pred$class, iris[,5])


## Example 2; boosting{adabag}
library(adabag)
data(iris)
boo.adabag <- boosting(Species~., data=iris, boos=TRUE, mfinal=10)
boo.adabag$importance

plot(boo.adabag$trees[[10]])
text(boo.adabag$trees[[10]])

pred <- predict(boo.adabag, newdata=iris)
tb <- table(pred$class, iris[,5])
tb
error.rpart <- 1-(sum(diag(tb))/sum(tb))
error.rpart


## Example 3; 
library(ada)
data(iris)
iris[iris$Species!="setosa", ] -> iris # setosa 50
n <- dim(iris)[1]
trind <- sample(1:n, floor(.6*n), FALSE)
teind <- setdiff(1:n, trind) # set difference( )
iris[,5] <- as.factor((levels(iris[, 5])[2:3])[as.numeric(iris[,5])-1])
gdis<-ada(Species~., data=iris[trind,], iter=20, nu=1, type="discrete")
  # nu=1(default) shrinkage parametor
  # type=¡°discrete¡±( ) . ¡°real¡±, ¡°gentle¡±
gdis<-addtest(gdis, iris[teind, -5], iris[teind, 5])
gdis

plot(gdis, TRUE, TRUE)
varplot(gdis)
pairs(gdis, iris[trind,-5], maxvar=4)


## Example 4; randomForest{randomForest}
library(randomForest)

data(stagec)
str(stagec)
stagec1<- subset(stagec, !is.na(g2))
stagec2<- subset(stagec1, !is.na(gleason))
stagec3<- subset(stagec2, !is.na(eet))
str(stagec3)
set.seed(1234)
ind <- sample(2, nrow(stagec3), replace=TRUE, prob=c(0.7, 0.3)); ind
trainData <- stagec3[ind==1, ]
testData <- stagec3[ind==2, ]
str(trainData);str(testData)

rf <- randomForest(ploidy ~ ., data=trainData, ntree=100,
                   proximity=TRUE) # proximity=TRUE : matirix
table(predict(rf), trainData$ploidy)
print(rf)
plot(rf)

importance(rf)
varImpPlot(rf)

rf.pred <- predict(rf, newdata=testData)
table(rf.pred, testData$ploidy)
plot(margin(rf))

## random forest using cforest{party}
library(party)
set.seed(1234)
cf <- cforest(ploidy ~ ., data=trainData)
cf.pred <- predict(cf, newdata=testData, OOB=TRUE, type="response") # out of bag


## Example5 rf using {caret}
require(caret)
require(ggplot2)
require(randomForest)

training_URL<-"http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_URL<-"http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training<-read.csv(training_URL, na.strings=c("NA","") )
test<-read.csv(test_URL, na.strings=c("NA","") )
str(training) # a lot var
str(test) 
training<-training[,7:160]
test<-test[,7:160]
mostly_data <- apply(!is.na(training), 2, sum)>19621 # be careful!
training <- training[,mostly_data]
test <- test[,mostly_data]
dim(training)

InTrain<-createDataPartition(y=training$classe, p=0.3, list=FALSE)
training1<-training[InTrain,]

rf_model<-train(classe~., data=training1, method="rf",
                trControl=trainControl(method="cv", number=5),
                prox=TRUE, allowParallel=TRUE)
print(rf_model)
print(rf_model$finalModel)
