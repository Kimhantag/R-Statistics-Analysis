# 20장 회귀분석
# 예제 1 : 단순회귀분석
library(MASS)
attach(cats)
lm.out<-lm(Hwt ~ Bwt)
summary(lm.out)

options(show.signif.stars = F)
anova(lm.out)

plot(Hwt ~ Bwt, main = "Kitty Cat Plot")
abline(lm.out, col='red')

par(mfrow=c(2,2))
plot(lm.out)

cats[144,]
lm.out$fitted.values[144]
lm.out$residuals[144]

par(mfrow=c(1, 1))
plot(cooks.distance(lm.out))

lm.without144 <- lm(Hwt ~ Bwt, subset=(Hwt <20.5))
lm.without144

rlm(Hwt ~ Bwt)
plot(Hwt ~ Bwt)
lines(lowess(Hwt ~ Bwt), col='red')
detach(cats)

# 예제 2 : 다중회귀분석
data(state)
st<-as.data.frame(state.x77)
str(st)
colnames(st)[4] <- "Life.Exp"
colnames(st)[6] <- "HS.Grad"

st[, 9]<- st$Population*1000/st$Area
colnames(st)[9] <- "Density"
str(st)
summary(st)
cor(st)
pairs(st)

model1<-lm(Life.Exp ~ ., data=st)
summary(model1)
BIC(model1)

model2<-update(model1, .~.-Illiteracy)
summary(model2)
BIC(model2)

model3<-update(model2, .~.-Area)
summary(model3)
BIC(model3)

model4<-update(model3, .~.-Income)
summary(model4)
BIC(model4)

model5<-update(model4, .~.-Density)
summary(model5)
BIC(model5)

model6<-update(model5, .~.-Population)
summary(model6)
BIC(model6)

model.step<-step(model1, direction = 'backward')
summary(model.step)

confint(model.step)

predict(model.step, list(Population=4000, Murder=10.5, HS.Grad=48, Frost=100))

par(mfrow=c(2,2))
plot(model.step)

names(model.step)
model.step[[1]]
model.step[[2]]
sort(model.step$resid)
           
model.beta<-lm(Life.Exp ~ scale(Population) + scale(Murder) + scale(HS.Grad)
               + scale(Frost), data=st)
summary(model.beta)

# 연습문제1
data(trees)
lm_trees<-lm(Volume ~ Girth, data=trees)
summary(lm_trees)
predict(lm_trees, list(18))
par(mfrow=c(2,2))
plot(lm_trees)

# 연습문제2
lm_height<-lm(Volume ~ Height, data=trees)
summary(lm_height)
