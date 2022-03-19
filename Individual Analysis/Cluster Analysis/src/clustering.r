library("MASS")
library(StatMatch)
library(factoextra)
library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)
library(scatterplot3d)
library(carData)
library(car)
library("scatterplot3d")
library(rgl)
library(nlme)
library(mgcv)

# load data
data = read.csv("clean_data.csv")
data = data[, 3:28]
data = data[ , ! names(data) %in% c("Dt_Customer")] 

# pre-processing, convert column types
head(data)
sapply(data, class)
data$Age <- as.numeric(data$Age)
data[4:25] <- lapply(data[4:25], as.numeric)
data$Education <- as.factor(data$Education)
data$Marital_Status <- as.factor(data$Marital_Status)
sapply(data, class)
plot(data)
qplot(data$Income, data$NumWebVisitsMonth, geom = "point")
# MDS
d = gower.dist(data)
d = pmax(d, 0.000000000000001)
fit = isoMDS(d, k=2)
fit
plot(fit$points, xlab = "Coordinate 1", ylab = "Coordinate 2",main = "Marketing MDS")
fit$stress

# reduce the dimension through PCA base on previous PCA analysis

#calculate how many clusters you need
sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(d, diss = TRUE, k = i) 
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, sil_width,
     xlab = "Number of Clusters",
     ylab = "Silhouette Width",
     main = "Clusters Silhouette Plot")
lines(1:8, sil_width)

# visualization
tsne_obj <- Rtsne(d, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) + labs(title="Clustering Plot",
                                          x ="Coordinate 1", y = "Coordinate 2")

# reduced dimension (PCA)
data.reduced = data[c("Income", "Kidhome", "MntWines", "MntFruits", "MntMeatProducts", "MntFishProducts", "MntSweetProducts", 
                      "MntGoldProds", "NumWebVisitsMonth", "NumDealsPurchases", "NumWebPurchases", "AcceptedCmp2", "AcceptedCmp3",
                      "AcceptedCmp4", "Age", "Teenhome")]
data.reduced.scale = scale(data.reduced)
data.dist = dist(data.reduced.scale)
  
# calculate how many clusters you need
fviz_nbclust(data.reduced.scale, kmeans, method = "wss") + labs(subtitle = "Elbow Method") # 3 clusters

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(data.dist, method = "complete" )

# MDS
data.dist = pmax(data.dist, 0.000000000000001)
fit2 = isoMDS(data.dist, k=2)
plot(fit$points, xlab = "Coordinate 1", ylab = "Coordinate 2",main = "Reduced Marketing MDS")
fit2$stress
# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)
rect.hclust(hc1, k = 3, border = 2:5)
km.out = kmeans(data.reduced.scale, centers = 2, nstart = 100)
km.clusters = km.out$cluster
fviz_cluster(list(data = data.reduced.scale, cluster = km.clusters))

# Classsfiy two groups
hist(data$Income, xlim = c(0, 200000))
mean(data$Income) # 52237.98
data$income_class = ""
for(i in 1:nrow(data)){
  if (data[i, "Income"] > 52237.98) {
    data[i, "income_class"] = "Higher"
  } else {
    data[i, "income_class"] = "Lower"
  }
}

rownames(data.reduced.scale) <- paste(data$income_class, 1:dim(data)[1], sep = "_")
fviz_cluster(list(data = data.reduced.scale, cluster = km.clusters))

# new approach
d = dist(data.reduced) # rna would run a long, long time here
fit = cmdscale(d, eig=TRUE, k=2)
fit
xMDS = fit$points[,1]
yMDS = fit$points[,2]
plot(xMDS, yMDS, xlab="Coordinate 1", ylab="Coordiante 2", main="Metric MDS of Marketing Reduced Data", pch=16, cex=.5)

fit2 = isoMDS(d, k=2)
print(fit2)
fviz_nbclust(data.reduced, kmeans, method = "wss") + geom_vline(xintercept = 6, linetype = 2) + labs(subtitle = "Elbow method")
fviz_nbclust(data.reduced, kmeans, method = "silhouette") + labs(subtitle = "Silhouette method")

rnaClustK = kmeans(data.reduced, centers=6)
plot(xMDS, yMDS, xlab="Coordinate 1", ylab="Coordiante 2", main="Metric MDS of Marketing Reduced Data", pch=16, cex=.5, col=rnaClustK$cluster)

p = prcomp(data.reduced)
scatterplot3d(p$x[,1:3], color=rnaClustK$cluster)
scatter3d(x = p$x[,1], y = p$x[,2], z = p$x[,3], groups = as.factor(rnaClustK$cluster), grid = FALSE, surface = FALSE)
