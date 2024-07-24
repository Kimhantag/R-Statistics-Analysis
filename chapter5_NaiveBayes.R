##### Chapter 5 Naive Bayes classification

(height1 <- dnorm(6, mean=5.855, sd=sqrt(0.035033)))
weight1 <- dnorm(130, mean=176.26, sd=sqrt(122.92))
feet1 <- dnorm(8, mean=11.25, sd=sqrt(0.91667))
m.x <- 0.5*height1*weight1*feet1 
m.x

height2 <- dnorm(6, mean=5.4175, sd=sqrt(0.097225))
weight2 <- dnorm(130, mean=132.5, sd=sqrt(558.33))
feet2 <- dnorm(8, mean=7.5, sd=sqrt(1.6667))
f.x <- 0.5 * height2 * weight2 * feet2 
f.x

## exmaple1 naiveBayes{e1071}
data(iris)
head(iris)
library(e1071)
m <- naiveBayes(Species ~ ., data = iris)
m
table(predict(m, iris), iris[,5])


## exmaple2 NaiveBayes{klaR}
install.packages("https://cran.r-project.org/src/contrib/Archive/ElemStatLearn/ElemStatLearn_2015.6.26.tar.gz",
                 repos = NULL,
                 type = "source")
library(ElemStatLearn)
data(spam)
str(spam)
library(klaR)

train.ind <- sample(1:nrow(spam), ceiling(nrow(spam)*2/3),
                    replace=FALSE)
nb.res <- NaiveBayes(spam ~ ., data=spam[train.ind,])
opar <- par(mfrow=c(2,4))
plot(nb.res)

par(opar)
nb.pred <- predict(nb.res, spam[-train.ind,])
confusion.mat <- table(nb.pred$class, spam[-train.ind,"spam"])
confusion.mat
sum(diag(confusion.mat))/sum(confusion.mat) 


## exsample2 {kernlab} 사용하기
library(kernlab)
data(spam)
str(spam)
library(klaR)
train.ind <- sample(1:nrow(spam), ceiling(nrow(spam)*2/3),
                    replace=FALSE)
nb.res <- NaiveBayes(type ~ ., data=spam[train.ind,])
opar <- par(mfrow=c(2,4))
plot(nb.res)

par(opar)
nb.pred <- predict(nb.res, spam[-train.ind,])
confusion.mat <- table(nb.pred$class, spam[-train.ind,"type"])
confusion.mat
sum(diag(confusion.mat))/sum(confusion.mat) 
  # acc, spc, sen!


## exmaple3 NaiveBayes{klaR}
library (e1071)
data (HouseVotes84, package="mlbench")
head(HouseVotes84)
summary(HouseVotes84)

model <- naiveBayes(Class ~ ., data = HouseVotes84)
pred <- predict(model, HouseVotes84[,-1])
tab <- table(pred, HouseVotes84$Class)
tab # confusion table
table(HouseVotes84$Class) # prior
sum(tab[row(tab)==col(tab)])/sum(tab) # acc