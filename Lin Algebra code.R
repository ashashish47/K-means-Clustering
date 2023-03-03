
# Installing Packages
install.packages("ClusterR")
install.packages("cluster")

# Loading package
library(ClusterR)
library(cluster)

# Importing Dataset from Excel File
data=read.csv("Maths Assignment Data.csv")
data
#Extracting Values
data1=data[c(1:118),c(6,7,8,9)]
data1

library(factoextra)
#create plot of number of clusters vs total within sum of squares
fviz_nbclust(data1, kmeans, method = "wss")

# Fitting K-Means clustering Model 
set.seed(10) # Setting seed
kmeans.re <- kmeans(data1, centers = 3, nstart = 20)
kmeans.re

#create cluster plot
fviz_cluster(kmeans.re,data1)

#Fitted clusters corresponding to each vector
fitted(kmeans.re,method="classes")

#Total within sum of squares
kmeans(data1, centers = 5, nstart = 20)$tot.withinss





library("plot3D")
scatter3D(data1$D1,data1$D2,data1$D3)
# Cluster identification for 
# each observation
kmeans.re$cluster

# Confusion Matrix
cm <- table(iris$Species, kmeans.re$cluster)
cm

# Model Evaluation and visualization
plot(iris_1[c("Sepal.Length", "Sepal.Width")])
plot(iris_1[c("Sepal.Length", "Sepal.Width")], 
     col = kmeans.re$cluster)
plot(iris_1[c("Sepal.Length", "Sepal.Width")], 
     col = kmeans.re$cluster, 
     main = "K-means with 3 clusters")

## Plotiing cluster centers
kmeans.re$centers
kmeans.re$centers[, c("Sepal.Length", "Sepal.Width")]

# cex is font size, pch is symbol
points(kmeans.re$centers[, c("Sepal.Length", "Sepal.Width")], 
       col = 1:3, pch = 8, cex = 3) 

## Visualizing clusters
y_kmeans <- kmeans.re$cluster
clusplot(iris_1[, c("Sepal.Length", "Sepal.Width")],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Cluster iris"),
         xlab = 'Sepal.Length',
         ylab = 'Sepal.Width')