library("caret")

dados <- read.csv(file = '/Users/MPPR/Documents/Pos_IA/pos_ia_aprendizado_maquina/banco/banco.csv')
dados_novos <- read.csv(file = '/Users/MPPR/Documents/Pos_IA/pos_ia_aprendizado_maquina/banco/banco_novos.csv')

set.seed(728078902)

ran <- sample(1:nrow(dados), 0.8 * nrow(dados))
treino <- dados[ran,] 
teste <- dados[-ran,] 

### Faz um grid com valores para K e Executa o KNN
tuneGrid <- expand.grid(k = c(1,3,5,7,9))

knn <- train(y~., data = treino, method = "knn",tuneGrid=tuneGrid)
knn

### Faz a predição e mostra a matriz de confusão
predict.knn <- predict(knn, teste)
confusionMatrix(predict.knn, as.factor(teste$y))

### PREDIÇÕES DE NOVOS CASOS

predict.knn <- predict(knn, dados_novos)
dados_novos$y <- NULL
dados_novos <- cbind(dados_novos, predict.knn)
