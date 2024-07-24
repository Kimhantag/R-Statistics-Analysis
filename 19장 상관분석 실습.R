# 19장 상관분석

# 예제1
data(longley)
str(longley)
cor(longley) #피어슨 상관계수
pairs(longley)

cor(longley, method="spearman") #스피어만 상관계수
cor(longley, method="kendall") #캔달의 상관계수

# 예제2
library("MASS")
data(cats)
str(cats)
summary(cats)
sum(is.na(cats))

with(cats, plot(Bwt, Hwt))
title(main="Heart Weight (g) vs. Body Weight (kg) \nof Domestic Cats")

with(cats, cor(Bwt, Hwt))
with(cats, cor(Bwt, Hwt))^2

# cor.test : 상관계수에 대한 검정
with(cats, cor.test(Bwt, Hwt))
with(cats, cor.test(~Bwt + Hwt, subset = (Sex=="F")))

with(cats, plot(Bwt, Hwt, type="n", xlab="Body Weight in kg",
                ylab="Heart weight in g",
                main="Heart Weight vs. Body Weight of Cats"))
with(cats, points(Bwt[Sex=="F"], Hwt[Sex=="F"], pch=16, col="red"))
with(cats, points(Bwt[Sex=="M"], Hwt[Sex=="M"], pch=17, col="blue"))

# 상관분석 시각화
# psych 패키지: pairs.panel(), cor.plot()
install.packages("psych")
library(psych)

# pairs.panels() 함수 적용의 예
data(iris)
pairs.panels(iris[1:4], scale=T)

pairs.panels(iris[1:4], bg=c("red", "yellow", "blue")[iris$Species],
             pch=21, main="Fisher Iris data by Species")

# cor.plot() 함수의 적용 예
cor.plot(cor(mtcars))

# corrplot 패키지: corrplot()
install.packages("corrplot")
library(corrplot)

# corrplot 함수의 적용 예
M<-cor(mtcars)
corrplot(M, method="ellipse", type="lower") #하대각 위치 타원형

#corrplot.mixed 함수의 적용의 예
corrplot(M)
corrplot(M, order="hclust", addrect = 3)
corrplot.mixed(M)

#컬러 스펙트럼 변경하기
col.1<-colorRampPalette(c("red", "white", "blue"))
wb<-c("white", "black")

par(mfrow=c(1,2))
corrplot(M, order="hclust", addrect=2, col=col.1(20))
corrplot(M, order="hclust", addrect=2, col=wb, bg="gold2")

install.packages("GGally")
library(GGally)
data(tips, package="reshape")
ggpairs(data=tips, columns = 1:3,
        title = "tips data", mapping=ggplot2::aes(color=sex))
pm<-ggpairs(data=tips, columns = 1:3,
            upper=list(continuous="density"), lower=list(combo="facetdensity"),
            title = "tips data", mapping=ggplot2::aes(color=sex))
pm
cp<-ggplot(data.frame(x=1:10, y=1:10))+geom_point(aes(x,y))
putPlot(pm, cp, 2, 3)

getPlot(pm, 2, 1)

# 연습문제 1
# 1-(a)
data(trees)
attach(trees)
str(trees)
# 산점도 행렬
pairs(trees)
# 변수 간 상관계수
cor(trees)

# 1-(b)
par(mfrow=c(2,2))
library(psych)
pairs.panels(trees, scale=T)

# 1-(c)
library(GGally)
ggpairs(trees)

# 연습문제 2
library(MASS)
data(Cars93)
str(Cars93)
attach(Cars93)

#연습문제3
data(mtcars)
attach(mtcars)
str(mtcars)
cor(hp, mpg)

cor.test(hp, mpg, method="pearson")
cor.test(hp, mpg, method="spearman")
cor.test(hp, mpg, method="kendall")
