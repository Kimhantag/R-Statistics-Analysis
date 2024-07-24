##### Chapter 13 clustering

## Example1 hclust{stats}
data(USArrests)
str(USArrests)
d <- dist(USArrests, method="euclidean") # method
fit <- hclust(d, method="ave")

par(mfrow=c(1,2))
plot(fit)
plot(fit, hang = -1)
par(mfrow=c(1,1))

groups <- cutree(fit, k=6) #cutree() make the group
groups

plot(fit)
rect.hclust(fit, k=6, border="red") # red box group

hca <- hclust(dist(USArrests))
plot(hca)
rect.hclust(hca, k = 3, border = "red")
rect.hclust(hca, h = 50, which = c(2,7), border = 3:4) # 9 grup


## Example2 agnes{cluster}
library(cluster)
# check difference agn1 vs agn2 vs agn3
(agn1 <- agnes(USArrests, metric="manhattan", stand=TRUE))
plot(agn1)

agn2 <- agnes(daisy(USArrests), diss=TRUE, method="complete")
plot(agn2)

agn3 <- agnes(USArrests, method ="flexible", par.meth=0.6)
plot(agn3)


## Example3 hclust object
fit <- hclust(d, method="ave")
clus6 = cutree(fit, 6)
colors = c("red", "blue", "green", "black", "cyan", "magenta")
library(ape)
plot(as.phylo(fit), # phylo object
     type = "fan", tip.color = colors[clus6], label.offset = 1, cex = 0.7)
plot(as.phylo(fit), type = "cladogram", #type = "cladogram"
     cex = 0.6, label.offset = 0.5)
plot(as.phylo(fit), type = "unrooted", #type = "unrooted"
     cex = 0.6, no.margin = TRUE)

## k-mean cluster
wssplot <- function(data, nc=15, seed=1234){ #nc max k
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}


## Example4
library("rattle.data")
data(wine, package="rattle.data")
head(wine)
df <- scale(wine[-1]) # for finding k
wssplot(df)

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
par(mfrow=c(1,1))
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

set.seed(1234)
fit.km <- kmeans(df, 3, nstart=25) # nstart= ´ÙÁßÀÇ ÃÊ±ê°ª ¼ö
fit.km$size
fit.km$centers

plot(df, col=fit.km$cluster)
points(fit.km$center, col=1:3, pch=8, cex=1.5) # center of cluster

aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean)

ct.km <- table(wine$Type, fit.km$cluster)
ct.km

library(flexclust)
randIndex(ct.km) # adjusted Rand index


## Example5 
library(flexclust)
data("Nclus")
plot(Nclus)

cl <- kcca(Nclus, k=4, family=kccaFamily("kmeans"))
image(cl)
points(Nclus)
barplot(cl)
stripes(cl)


## Example6
library(cclust)
cl.1 <- cclust(Nclus, 4, 20, method="kmeans") 
plot(Nclus, col=cl.1$cluster)
points(cl.1$center, col = 1:4, pch = 8, cex=1.5) #center

library(cluster)
clusplot(Nclus, cl.1$cluster)
