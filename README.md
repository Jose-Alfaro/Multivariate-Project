# Multivariate-Project
Stat 7331 (Multivariate) Final Project  
Created: 11/6/2019  
Authors: Jose Alfaro and Kevin Lutz

#Problem 2 (part III of paper):  Factor Analysis, Canonical Correlation, Clustering

#Factor Analysis
library(psych)
library(parallel)
library(DataExplorer)
prob2 = data.st[,c(2,3,7:10,12,14,15)]
plot_correlation(prob2a)
prob2.vss = vss(prob2[,-9], n.obs=dim(data)[1])
prob2.vss
plot(prob2.vss)
fa.parallel(prob2[,-9])

factanal(prob2[,-9], factors = 2, rotation = "varimax")
factanal(prob2[,-9], factors = 3, rotation = "varimax")
factanal(prob2[,-9], factors = 4, rotation = "varimax")

#Canonical Correlation
library(ggplot2)
library(CCA)
library(GGally)

dat1 = prob2[,c(1,2,4,6,7)]
dat2 = prob2[,c(3,5,8)]
ggpairs(dat1)
ggpairs(dat2)

cc1 = cc(dat1, dat2)
cc1[3:4]
cc1$cor
cc2 = comput(dat1, dat2, cc1)
cc2[3:6]



source("http://www.statpower.net/R312/CanCorr.r")
output <- canonical.cor(dat1, dat2)
output[1]

s1 <- diag(sqrt(diag(cov(dat1))))
s2 <- diag(sqrt(diag(cov(dat2))))

s1 %*% cc1$xcoef
s2 %*% cc1$ycoef


#Clustering

data.std = data.st[,c(2,3,7:10,12,14)]
hc.complete = hclust(dist(data.std), method = "complete")

library(factoextra)
#K-Means Clustering with K = 2-8
kmeans.2 = kmeans(data.std, 2)
fviz_cluster(kmeans.2, data.std)

summary(prcomp(prob2[,1:8]))
data.pca = prcomp(prob2[,-9])
data.pca$rotation[,1]
data.pca$rotation[,2]
screeplot(data.pca, type = "lines", main = "Scaled PCA")
plot(data.pca$x[,1],data.pca$x[,2])
library(FactoMineR)
PCA(prob2[,1:8])

library(lattice)
xyplot(data.pca$x[,2] ~ data.pca$x[,1], group=data.std$Date, data= data.std, 
       auto.key=list(space="right"), 
       jitter.x=TRUE, jitter.y=TRUE,
       xlab = "First Principal Component",
       ylab = "Second Principal Component",
       main = "PCA Plot")


# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(prob2[,-9], k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 8
k.values <- 1:8
library(purrr)
# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#Silhouette Plot
fviz_nbclust(prob2[,-9], kmeans, method = "silhouette")
