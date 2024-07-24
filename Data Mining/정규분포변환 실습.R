# 정규 분포 변환
# qqnorm() 함수 적용 예: 난수이용
y<-rt(200, df=5)
par(mfrow=c(1,2))
qqnorm(y); qqline(y, col=2)
qqplot(y, rbinom(200, 1000, p=.5))

set.seed(1000)
x.norm<-rt(5000, df=50)
shapiro.test(x.norm)

x.binom<-rbinom(100, 5000, 0.5)
shapiro.test(x.binom)

# 예제1 박스-콕스 변환
library(MASS)
library(car)

x<-rexp(1000)
par(mfrow=c(1,2))
hist(x)
qqnorm(x)
par(mfrow=c(1,1))

boxcox(x~1)

p<-powerTransform(x)
y<-bcPower(x, p$lambda)
par(mfrow=c(1,2))
hist(y)
qqnorm(y)

p$lambda

#예제2 선형모형에서 박스콕스 변환
install.packages("faraway")
library(faraway)
data(ozone)
head(ozone)

md<-lm(O3~temp+humidity+ibh, data=ozone)
summary(md)

plot(md, which=1)

library(MASS)
bc<-boxcox(md, plotit=T)
bc<-boxcox(md, plotit=T, lambda=seq(0,0.8,by=0.1))

which.max(bc$y)
(lambda <- bc$x[which.max(bc$y)])

md_best <- lm(O3^lambda ~ )


# 연습문제1
attach(morley)
qqnorm(Speed)
qqline(Speed)
shapiro.test(Speed)

# 연습문제2
# (a)
data(trees)
attach(trees)
qqnorm(Volume)
qqline(Volume)
shapiro.test(Volume) # 정규분포를 따르지 않는다.

# (b)
library(car)
p<-powerTransform(Volume)
y<-bcPower(Volume, p$lambda)
p$lambda
hist(y)
qqnorm(y)
qqline(y)

# (c)
shapiro.test(y) # 정규성 만족

# 연습문제3
data(mtcars)
str(mtcars)
attach(mtcars)

# (a)
mt<-lm(mpg~hp+wt)
summary(mt)
plot(mt, which=1)

# (b) 정규분포를 따르지 않음
# (c)
bc<-boxcox(mt, plotit=T)
bc<-boxcox(mt, plotit=T, lambda=seq(-0.8, 0, 0.1))
which.max(bc$y) # 최대 lambda의 자료번호
(lambda <- bc$x[which.max(bc$y)])

#(d)
mm_best<-lm(mpg^lambda ~ hp+wt)
summary(mm_best)
plot(mm_best, which=1)
