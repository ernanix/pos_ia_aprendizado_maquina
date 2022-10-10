##Maquina MP
setwd('C:\\Users\\escneto\\Documents\\Estudos\\Pos_IA_UFPR\\pos_ia_aprendizado_maquina\\Bases_de_teste\\AGRUPAMENTO')
##Note
setwd('/Users/MPPR/Documents/Pos_IA/pos_ia_aprendizado_maquina/Bases_de_teste/AGRUPAMENTO')

data("iris")
dados <- iris
set.seed(728078902)
irisCluster <- kmeans(iris,3)
irisCluster
resultado <- cbind(dados,irisCluster$cluster)
resultado

############################

library(klaR)
dados <- read.csv(file = 'moveis.csv')
set.seed(728078902)
moveisCluster <- kmodes(dados,10,iter.max = 10, weighted = FALSE)
moveisCluster
resultado <- cbind(dados,moveisCluster$cluster)
resultado

############################

library(klaR)
dados <- read.csv(file = 'cancer_mama.csv')
set.seed(728078902)
cancerCluster <- kmeans(dados[2:10],2)
cancerCluster
resultado <- cbind(dados,cancerCluster$cluster)
resultado[2:12]

