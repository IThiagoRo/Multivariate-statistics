uno <- read.table("Data/base.txt", header=T)

genera <- function(cedula){
  set.seed(cedula)
  data <- uno[sample(1:2100,120),]
  data
}

datos <- genera(79466)

library(FactoClass)
datos <- datos[, 2:10]
clusters <- ward.cluster(dist(datos), h.clust=1)
plot(clusters, ylab=NULL, xlab="Observaciones")
rect.hclust(clusters, k=2, border="red")
rect.hclust(clusters, k=3, border="blue")

library(factoextra)
fviz_nbclust(x=datos, FUNcluster = kmeans, method="silhouette")

cluster_tree <- cutree(clusters, k = 2)
fviz_cluster(list(data=datos, cluster=cluster_tree))


m_cor <- cor(datos)
m_dist <- as.dist(1 - cor(datos)^2)
cluster  <- hclust(m_dist, method = "ward")
plot(cluster)
rect.hclust(cluster, k=2)

library(ClustOfVar)
library(factoextra)
tree <- hclustvar(datos)
plot(tree)
rect.hclust(tree, k=2, border=10:15) 
