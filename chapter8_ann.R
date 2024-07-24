##### Chapter 8 Artificial Neural network

## Example 1; nnet{nnet}
library(nnet)
data(iris)
nn.iris <- nnet(Species~., data=iris, size=2, rang=0.1,
                  decay=5e-4, maxit=200)
  # # of node 2
  # initial weight range (-0.1, 0.1)
  # max repeat time 200
  # weight decay default=0, speed of change weight
summary(nn.iris)

library(devtools)
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
plot.nnet(nn.iris)

# other tool
library(clusterGeneration)
library(scales)
library(reshape)
plot(nn.iris)

table(iris$Species, predict(nn.iris, iris, type = "class"))


## Example 2; neuralnet{neuralnet}
data(infert, package="datasets")
str(infert)
library(neuralnet)
net.infert <- neuralnet(case~age+parity+induced+spontaneous,
                      data=infert, hidden=2, err.fct="ce",
                      linear.output=FALSE, likelihood=TRUE)
  # hidden= # of hidden node
  # err.fct= (error) ¡°sse¡±, ¡°ce¡±(cross entropy)
  # likelihood=T ; PRINT likelihood AIC, BIC
net.infert
plot(net.infert)
names(net.infert)
net.infert$result.matrix
out <- cbind(net.infert$covariate, net.infert$net.result[[1]])
dimnames(out) <- list(NULL, c("age","parity","induced", "spontaneous", "nn-output"))
head(out)
head(net.infert$generalized.weights[[1]])

par(mfrow=c(2,2))
gwplot(net.infert, selected.covariate="age", min=-2.5, max=5)
gwplot(net.infert, selected.covariate="parity", min=-2.5, max=5)
gwplot(net.infert, selected.covariate="induced", min=-2.5, max=5)
gwplot(net.infert, selected.covariate="spontaneous", min=-2.5, max=5)
par(mfrow=c(1,1))

new.output <- compute(net.infert, covariate=matrix(c(22,1,0,0,
                                                     22,1,1,0,
                                                     22,1,0,1,
                                                     22,1,1,1),
                                                   byrow=TRUE, ncol=4))
new.output$net.result


## Example 3; neuralnet{neuralnet} multiple layer
library(neuralnet)
train.input <- as.data.frame(runif(50, min=0, max=100))
train.output <- sqrt(train.input)
train.data <- cbind(train.input, train.output)
colnames(train.data) <- c("Input","Output")
head(train.data)
net.sqrt <- neuralnet(Output~Input,train.data, hidden=10, threshold=0.01)
print(net.sqrt)
plot(net.sqrt)
test.data <- as.data.frame((1:10)^2)
test.out <- compute(net.sqrt, test.data)
ls(test.out)
print(test.out$net.result)

net2.sqrt <- neuralnet(Output~Input,train.data, hidden=c(10,8), threshold=0.01)
plot(net2.sqrt)
test2.out <- compute(net2.sqrt, test.data)
print(test2.out$net.result)
