library("caret")

##Maquina MP
setwd('C:\\Users\\escneto\\Documents\\Estudos\\Pos_IA_UFPR\\pos_ia_aprendizado_maquina\\Bases_de_teste')

dados <- read.csv(file = 'previsao_tempo\\previsao_tempo.csv')
dados_novos <- read.csv(file = 'previsao_tempo\\previsao_tempo_novos.csv')

### Set Seed
set.seed(728078902)

########################## KNN
### Cria arquivos de treino e teste
ran <- sample(1:nrow(dados), 0.8 * nrow(dados))
treino <- dados[ran,] 
teste <- dados[-ran,] 

### Faz um grid com valores para K e Executa o KNN
tuneGrid <- expand.grid(k = c(1,3,5,7,9))

knn <- train(Chovera~., data = treino, method = "knn",tuneGrid=tuneGrid)
knn

### Faz a predição e mostra a matriz de confusão
predict.knn <- predict(knn, teste)
confusionMatrix(predict.knn, as.factor(teste$Chovera))

### PREDIÇÕES DE NOVOS CASOS

predict.knn <- predict(knn, dados_novos)
dados_novos$Chovera <- NULL
dados_novos <- cbind(dados_novos, predict.knn)
########################## KNN