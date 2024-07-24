#카이제곱 검정 실습
#예제 1 적합도 검정
data(HairEyeColor)
HairEyeColor

Eye<-margin.table(HairEyeColor, 2)
Eye

chisq.test(Eye, p=c(.5, .25, .15, .1))

#예제 2 독립성 검정
HairEye<-margin.table(HairEyeColor, c(1,2))
HairEye

chisq.test(HairEye)

#예제 3 동일성 검정
a<-margin.table(HairEyeColor, c(3,2))
a
chisq.test(a)

b<-margin.table(HairEyeColor, 1)
b

#연습문제1
chisq.test(c(62, 71, 56, 65, 67, 82, 73))
chisq.test(c(62, 71, 56, 65, 67))

#연습문제2
belt<-matrix(c(72, 18, 6, 32), nrow = 2, ncol = 2, byrow = TRUE)
dimnames(belt)<-list(c("O","X"),c("O","X"))
belt
chisq.test(belt)

#연습문제3
eye_test<-matrix(c(0, 2, 8, 2, 0, 0, 2, 3, 5, 2), nrow=2, ncol = 5, byrow = TRUE)
dimnames(eye_test)<-list(c("X", "O"), c(1, 2, 3, 4, 5))
eye_test
chisq.test(eye_test)
fisher.test(eye_test)

#예제 4 분할표 만들기
esoph
str(esoph)
xtabs(cbind(ncases, ncontrols)~., data=esoph)
ftable(xtabs(cbind(ncases, ncontrols)~., data=esoph))
ftable(xtabs(cbind(ncases, ncontrols)~agegp, data=esoph))
esoph$ncases
esoph$ncontrols
esoph

#예제 5 xtabs 카이제곱 검정
UCBAdmissions
DF<-as.data.frame(UCBAdmissions)
DF
xtabs(Freq ~ Gender + Admit, DF)
summary(xtabs(Freq ~ ., DF))

#예제 6 CrossTable{gmodels} 함수
str(infert)
install.packages("gmodels")
library(gmodels)
data(infert, package="datasets")
CrossTable(infert$education, infert$induced, expected=TRUE, dnn=c("Education", "Induced"))

#예제 7 gofTest(): 적합도 검정
install.packages("EnvStats")
library(EnvStats)
set.seed(1020)
x<-rexp(50, rate=1.0)
gofTest(x, test="chisq")

library(MASS)
fitdistr(x, "normal")

#예제 8 ks.test(): 콜모고로프 스미르노프 검정(비모수적 방법)
require(graphics)
x<-rnorm(50)
y<-runif(50)

#예제 8-1 x, y가 같은 분포로부터 나왔는지 검정
ks.test(x, y)

#예제 8-2 x+2가 Gamma(3,2) 분포로부터 나왔는지를 검정
ks.test(x+2, "pgamma", 3, 2)
ks.test(x+2, "pgamma", 3, 2, exact=FALSE)
ks.test(x+2, "pgamma", 3, 2, alternative = "gr")

#예제 8-3 x가 x+2보다 확률적으로 큰지를 검정
x2<-rnorm(50, -1)
plot(ecdf(x), xlim=range(c(x, x2)))
plot(ecdf(x2), add=TRUE, lty="dashed")

#모수적 검정
t.test(x, x2, alternative = "g")
#비모수적 검정
wilcox.test(x, x2, alternative = "g")

ks.test(x, x2, alternative = "l")

morley
str(morley)
install.packages("EnvStats")
library(EnvStats)
gofTest(morley$Speed, test="chisq")
ks.test(morley$Speed, "pnorm")
