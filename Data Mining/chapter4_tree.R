##### Chapter 4 Tree

## exmaple1 rpart{rpart}
library(rpart)
c <- rpart(Species ~., data=iris) # rpart{rpart}
c
ls(c)

plot(c, compress=T, margin=0.3)
text(c, cex=1.5)

head(predict(c, newdata=iris, type="class"))
tail(predict(c, newdata=iris, type="class"))

library(rpart.plot)
prp(c, type=4, extra =2) # prp{rpart.plot}

ls(c)

c$cptable
  # cost-complex parameter and cv eroro
opt <- which.min(c$cptable[,"xerror"])
cp <- c$cptable[opt, "CP"]
  # choose CP(=0.01) with min xerror
prune.c <- prune(c, cp = cp)
  # prune with CP (with min xerror)

plot(prune.c)
text(prune.c, use.n=T)

plotcp(c)


## exmaple2 ctree{party}
library(party)
data(stagec)
str(stagec)

# subsitution missing value
stagec1<- subset(stagec, !is.na(g2))
stagec2<- subset(stagec1, !is.na(gleason))
stagec3<- subset(stagec2, !is.na(eet))
str(stagec3)

# divid trining:test=7:3
set.seed(1234)
ind <- sample(2, nrow(stagec3), replace=TRUE, prob=c(0.7, 0.3))
ind
trainData <- stagec3[ind==1, ]
testData <- stagec3[ind==2, ]

tree <- ctree(ploidy ~ ., data=trainData)
tree
plot(tree)

testPred = predict(tree, newdata=testData)
table(testPred, testData$ploidy) # confusion table


## exmaple3 ctree{party} with cont
airq <- subset(airquality, !is.na(Ozone))
head(airq)
airct <- ctree(Ozone ~ ., data=airq) 
  # default pre-prune 
airct
plot(airct)
head(predict(airct, data=airq))
predict(airct, data=airq, type="node")
  # type="node"
mean((airq$Ozone - predict(airct))^2) # mean square error
