# Let us find the clusters in given Retail Customer Spends data
# Hierarchical Clustering

# Let us first set the working directory path and import the data

customerSpend <- read.csv("./data/Cust_Spend_Data.csv", header=TRUE)
View(customerSpend)

# Determine the euclidean distance
d.euc <- dist(x=customerSpend[,3:7], method = "euclidean") 
d.euc

# we will use the hclust function to build the cluster
?hclust  ## to get help on hclust function

hCluster <- hclust(d.euc, method = "average")

plot(hCluster, labels = as.character(customerSpend[,2]))

# scale function standardizes the values
scaled.RCDF <- scale(customerSpend[,3:7])
head(scaled.RCDF, 10)
d.euc <- dist(x=scaled.RCDF, method = "euclidean") 
hCluster <- hclust(d.euc, method = "average")
plot(hCluster, labels = as.character(customerSpend[,2]))
rect.hclust(hCluster, k=3, border="red")
clus2$height

View(customerSpend)
## profiling the clusters
customerSpend$Clusters <- cutree(hCluster, k=4)
View(customerSpend)

library("dplyr")
customer.profile <- group_by(customerSpend[,-c(1,2)], Clusters)
customer.profile <- summarise(customer.profile, count=n(), mean(Avg_Mthly_Spend), mean(No_Of_Visits), mean(Apparel_Items), mean(FnV_Items), mean(Staples_Items))
customer.profiles <- data.frame(customer.profile)
View(customer.profile)

  
## K Means Clustering

customerSpend <- read.csv("./data/Cust_Spend_Data.csv", header=TRUE)
## scale function standardizes the values
scaled.RCDF <- scale(customerSpend[,3:7])

# code taken from the R-statistics blog
# http://www.r-statistics.com/2013/08/k-means-clustering-from-r-in-action/

# Identifying the optimal number of clusters form WSS

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(scaled.RCDF, nc=6)

## Identifying the optimal number of clusters
## install.packages("NbClust")

library(NbClust)

set.seed(1234)
?NbClust()
nc <- NbClust(scaled.RCDF[,c(-1,-2)], min.nc=2, max.nc=4, method="kmeans")
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")


kmeans.clus = kmeans(x=scaled.RCDF, centers = 3, nstart = 25)
kmeans.clus

tmp <- as.data.frame(scaled.RCDF)
tmp$Clusters <- kmeans.clus$cluster
head(tmp)
## plotting the clusters
##install.packages("fpc")
library(fpc)
plotcluster(scaled.RCDF, kmeans.clus$cluster)

# More complex
library(cluster)
clusplot(scaled.RCDF, kmeans.clus$cluster, 
         color=TRUE, shade=TRUE, labels=2, lines=1)

## profiling the clusters
library("dplyr")
groupByClusters <- group_by(tmp, Clusters)
head(tmp)
summByClusters <- summarise(groupByClusters, count=n(), mean(Avg_Mthly_Spend), mean(No_Of_Visits), mean(Apparel_Items), mean(FnV_Items), mean(Staples_Items))
summByClusters <- data.frame(summByClusters)
summByClusters

predict(kmeans.clus, )
