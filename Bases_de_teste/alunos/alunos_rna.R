library(caret)
library(mlbench)
library(mice)
library(Metrics)

dados <- read.csv(file = '/Users/MPPR/Documents/Pos_IA/pos_ia_aprendizado_maquina/Bases_de_teste/alunos/alunos.csv')
dados_novos <- read.csv(file = '/Users/MPPR/Documents/Pos_IA/pos_ia_aprendizado_maquina/Bases_de_teste/alunos/alunos_novos.csv')

set.seed(728078902)

indices <- createDataPartition(dados$G3,p=0.8,list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]

rna <- train(G3 ~ .,data=treino,method="nnet",linout=T,trace=FALSE)

predicoes.rna <- predict(rna,teste)

rmse(teste$G3,predicoes.rna)
