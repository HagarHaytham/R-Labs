#Lab 2 Clustering
#---------------------------Requirement 1---------------------------------------#
#1-First of all, start by cleaning the workspace and setting the working directory.
rm(list=ls())
getwd()
setwd("D:\\CUFE\\4B\\Big Data\\Labs\\R-Labs\\Lab 2 - Clustering")
getwd()

#2-Import the dataset clustering_data.csv into a data frame and plot the points.
df <- read.csv("clustering_data.csv",header = TRUE)
plot(df)

#3-Perform a k-means clustering on the data with 10 clusters and 15 iterations.
set.seed(1234)
km <- kmeans(df,10,15)
km
#4-Print the cluster centroids.
km$centers
#5-Plot data such that each point is colored according to its cluster.
plot(df, col=km$cluster)
#6-Overlay the cluster centroids on the above plot. Plot them as solid filled triangles.
points(km$center,col=1:10,pch=10,cex=1)
#7-What's the difference between plot() and points()?

#8-Now, determine the best number of clusters by two different ways.
install.packages(c("HSAUR", "NbClust"))
library(rattle.data)
library(NbClust)
library(cluster)
library(HSAUR)
#First way
wss <- (nrow(df)-1)*sum(apply(df,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(df, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#second way
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")

#9-Tabulate the voting results.
table(nc$Best.n[1,])
#10-Perform a k-means clustering on the data with the best number of clusters chosen in step 3. Choose an appropriate number of iterations.
km2 <- kmeans(df,3,60)

#11-Repeat (4, 5, 6) for the new number of clusters.
km2$centers
plot(df, col=km$cluster)
points(km$center,col=1:4,pch=8,cex=1)


#---------------------------Requirement 1---------------------------------------#
#1- First of all, start by cleaning the workspace and setting the working directory.
rm(list=ls())
setwd("D:\\CUFE\\4B\\Big Data\\Labs\\R-Labs\\Lab 2 - Clustering")
#2-Read the image bird_small.png.
install.packages("png")
library(png)
image<-readPNG("bird_small.png")
#3-Create a data frame with three columns
NROW(image);NCOL(image)
r<-c(image[1:128,1:128,1])
g<-c(image[1:128,1:128,2])
b<-c(image[1:128,1:128,3])


dfimage <- data.frame(r,g,b)
colnames(dfimage) <- c("R","G","B") # variable names

#4-Perform a k-means clustering on the data with 16 clusters and 15 iterations.
kmimage <- kmeans(dfimage,16,15)

#5-Show the cluster each point belongs to.
plot(dfimage, col=kmimage$cluster)
#6-Display the 16 centroids of the clusters. What do these centroids represent?
kmimage$centers
#7-Assign each pixel to the centroid of its cluster.

#8-Reshape the data frame again to a 3-dimensional form.

#9-Write the compressed image in a file named compressed.png. Now check its size.
