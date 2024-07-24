##### Chapter2 Pre-processing & evaluation
library("caret")

## example 2
## 2.2.1 near-zero-variables
data(mdrr)
str(mdrrDescr)
str(mdrrClass)
data.frame(table(mdrrDescr$nR11)) 
  # check nR11 variable freq 

nzv <- nearZeroVar(mdrrDescr, saveMetrics = TRUE)
  # check option saveMetrics=T ; detail information
str(nzv)
nzv[nzv$nzv, ][1:10,]
dim(mdrrDescr) # # of obs. & var
nzv <- nearZeroVar(mdrrDescr) ;nzv
  # save near-zero-variables
filteredDescr <- mdrrDescr[, -nzv]
  # delete near-zero-variables
dim(filteredDescr)

## 2.2.2 cor
descrCor <- cor(filteredDescr) # correlation
(highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999))
  # # of vaiables with over abs 0.999
summary(descrCor[upper.tri(descrCor)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.75)
  # option cutoff
filteredDescr <- filteredDescr[, -highlyCorDescr]
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)])

## 2.2.3 pre-processing
## example 3
set.seed(200)
inTrain <- sample(seq(along = mdrrClass), length(mdrrClass)/2)
training <- filteredDescr[inTrain, ]
test <- filteredDescr[-inTrain, ]
trainMDRR <- mdrrClass[inTrain]
testMDRR <- mdrrClass[-inTrain]

# centering and scaling
preProcValues <- preProcess(training, method = c("center", "scale"))
  # option method "center", "scale", "ranges"
trainTransformed <- predict(preProcValues, training)
testTransformed <- predict(preProcValues, test)

## Example 4 
# box-cox trans
preProcValues2 <- preProcess(training, method = "BoxCox")
trainBC <- predict(preProcValues2, training)
testBC <- predict(preProcValues2, test)
preProcValues2

## 2.2.4 else
## Example 5 dummy var dummyVar{caret} vs dodel.matrix{stats} 
library(earth)
data(etitanic)
str(etitanic)

# model.matrix{stats} : # of (categories-1)
head(model.matrix(survived ~ ., data = etitanic))

# dummyVar{caret} : # of (categories)
dummy.1 <- dummyVars(survived ~ ., data = etitanic)
head(predict(dummy.1, newdata = etitanic))

## Example 6 Linear combinations
ltfrDesign <- matrix(0, nrow = 6, ncol = 6)
ltfrDesign[, 1] <- c(1, 1, 1, 1, 1, 1)
ltfrDesign[, 2] <- c(1, 1, 1, 0, 0, 0)
ltfrDesign[, 3] <- c(0, 0, 0, 1, 1, 1)
ltfrDesign[, 4] <- c(1, 0, 0, 1, 0, 0)
ltfrDesign[, 5] <- c(0, 1, 0, 0, 1, 0)
ltfrDesign[, 6] <- c(0, 0, 1, 0, 0, 1)

comboInfo <- findLinearCombos(ltfrDesign)
comboInfo
ltfrDesign[, -comboInfo$remove]

## Example 7 subsitution missing value
library(caret)
data(airquality); summary(airquality)
imp.1 <- preProcess(airquality, method=c("knnImpute"))
  # option method "knnImpute"
library(RANN)
imp.2 <- predict(imp.1, airquality); summary(airquality)

## Example 8 distance
trainSet <- sample(1:150, 100)
# classDist{caret}
distData <- classDist(iris[trainSet, 1:4], iris$Species[trainSet])
  # x & respnese y
  # option pca= , keep
distData$values

newDist <- predict(distData, iris[-trainSet, 1:4])
newDist

splom(newDist, groups = iris$Species[-trainSet], auto.key=list(columns=3))
  # scatter matrix

