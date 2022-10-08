data("iris")
dados <- iris

set.seed(728078902)
irisCluster <- kmeans(iris[,3:4],3)
irisCluster
resultado <- cbind(dados,irisCluster$cluster)
resultado
