library(cluster)
library(FNN)

data("iris")

x <- iris[,c(1:4)]
y <- iris[,5]

wssplot <- function(data, nc) {
  wss <- data.frame(value = numeric(0))
  for(i in 1:nc) {
    wss[i,1] <- kmeans(x, i)$tot.withinss
  }
  plot(1:nc, wss$value, type = "b")
}

# Variations within the group must be low  
# Best value of k is when the change in variation is not significant
wssplot(x, 10)

clusters <- kmeans(x, 3)

# Plot the cluster
clusplot(x, clusters$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

# identifying cluster membership for new data
  # https://stackoverflow.com/questions/20621250/simple-approach-to-assigning-clusters-for-new-data-after-k-means-clustering

# Use the nearnest neighbour to identify the cluster for new data
pred.cluster <- get.knnx(clusters$center, x, 1)$nn.index[,1]
all(clusters$cluster, pred.cluster)
