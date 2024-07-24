# 23장 통계모의실험
# 난수발생원리:역변환법
# 표준정규분포로부터 난수발생
set.seed(10)
r <- rnorm(10000, 0, 1)
summary(r)

hist(r, prob=T)
curve(dnorm(x), from=-4, to=4, add=T)

#이항분포로부터 난수 발생
n<-500
set.seed(1032)
U<-rbinom(n, 20, 0.5)
hist(U, breaks=0:20 - 1/2, border="red", col="pink",
     xlab="Random variates from B(20, 0.5) : n=500")

#이항분포의 확률밀도함수
x<-0:20
fx<-dbinom(x, 20, 0.5)
lines(x, n*fx, type="h", col="blue", lwd=2)
poins(x, n*fx, cex=2)

#예제1 
install.packages("IPSUR")
library(IPSUR)
set.seed(1000)
iqr<-replicate(10000, IQR(rnorm(100)))
hist(iqr)

summary(iqr)
quantile(iqr, 0.95)

#예제2
claim<-function(n, theta, alpha, lambda){
  n.claims <- rpois(n, lambda)
  S.N <- rep(0, n)
    for ( i in 1:n){
      u<-runif(n.claims[i])
      r.pareto <- theta * ((1-u)^(-1/alpha))
      S.N[i] <- sum(r.pareto)
      }
  S.N
}

claim.size <- claim(100000, 80000, 1.5, 1500)
profit.loss <- 5000*2-(claim.size/1000000+1000+50*2)

par(mfrow=c(1,2))
hist(claim.size/1000000, br=50, xlab="in millions", ylab="frequency",
     main="Distribution of Aggregate Claim", freq=FALSE)
hist(profit.loss, br=50, xlab="in millions", ylab="frequency",
     main="Distribution of Profit/Loss", freq=FALSE)

mean(claim.size/1000000)

#예제5
n <- c(1, 10, 30)
r<-10000

alpha<-0.5
beta<-1

mu<-alpha*beta
sigma<-sqrt(alpha)*beta

for (i in 1:length(n)){
  xbar<-rep(NA, r)
  sxbar<-sigma/sqrt(n[i])
  
  for (j in 1:r){
    x<-rgamma(n[i], shape=alpha, scale=beta)
    xbar[j]<-mean(x)
  }
  
  hist(xbar, prob=T)
  nor.pdf <- dnorm(seq(mu-3*sxbar, mu+3*sxbar, 0.1), mu, sxbar)
  lines(seq(mu-3*sxbar, mu+3*sxbar, 0.1), nor.pdf, lty=2, col="red")
}

# 예제 3
f<-function(x)
  sqrt(1-x^2)
s<-seq(-1, 1 ,by=0.01)
plot(s, f(s))

fun<-function(x) sqrt(1-x^2)
curve(fun, -1, 1)

c<-ceiling(max(f(s)))
c

n<-1000000
a<--1
b<-1
x<-runif(n, -1, 1)
y<-c*runif(n, 0, 1)
R<-sum(y < f(x)) / n
(b-a)*c*R

# 정확한 값
pi/2

# 예제 4
u<- runif(100000, min=2, max=5)
mean(u^5)*(5-2)

#정확한 값
f<-function(x) x^5
integrate(f, 2, 5)

#연습문제 1
set.seed(100)
xbar<-replicate(n= 10000, mean(rt(10, 3)))
xmed<-replicate(n= 10000, median(rt(10, 3)))
par(mfrow=c(1,2))
hist(xbar, xlim = c(-2, 2))
hist(xmed, xlim = c(-2, 2))
var(xbar)
var(xmed)

#연습문제2
par(mfrow=c(2,2))
for (i in 1:4){
  xbar=replicate(n= 10000, mean(runif(10*1^i, 0, 1)))
  hist(xbar)
}

f<-function(x) exp(-x)
s<-seq(0, 1, 0.001)
plot(s, f(s))
b=1; a=0
c<-ceiling(max(f(s)))
x<-runif(10000, 0, 1)
y<-c*runif(10000, exp(-1), 1)
R<-sum(y < f(x))/10000
(b-a) * c*R

integrate(f, 0, 1)

n=10000
g<-function(x) exp(-x)
u<-runif(n, 0, 1)
(b-a)*sum(g(u))*(1/n)
length(n)

n<-c(1, 10, 30)
r = 10000

alpha = 0.5
beta = 1
mu = alpha*beta
sigma = sqrt(alpha)*beta

for (i in 1:length(n)){
  xbar = rep(NA, r)
  sxbar = sigma / sqrt(n[i])
  for (j in 1:r){
    u<-rgamma(n[i], alpha, gamma)
    xbar[j] = mean(u)
  }
}

morley
shapiro.test(trees$Volume)
library(EnvStats)
gofTest(trees$Volume, test="chisq", method = "norm")

k<-lm(mpg ~ hp + wt, data=mtcars)
summary(k)
plot(k)
library(MASS)
library(car)

bc<-boxcox(k)
which.max(bc$y)
lambda<-bc$x[which.max(bc$y)]
lambda
k_log<-lm((mpg^lambda-1)/lambda ~ hp + wt, data=mtcars)
plot(k_log)
