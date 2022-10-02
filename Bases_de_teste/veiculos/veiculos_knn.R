library("caret")

setwd('C:\\Users\\escneto\\Documents\\Estudos\\Pos_IA_UFPR\\pos_ia_aprendizado_maquina\\Bases_de_teste')

dados <- read.csv(file = 'veiculos\\veiculos.csv')
dados_novos <- read.csv(file = 'veiculos\\veiculos_novos.csv')

### set seed
set.seed(728078902)

### retira id
dados$a <- NULL
dados_novos$a <- NULL


###Cria arquivo de treino e teste
ran <- sample(1:nrow(dados), 0.8 * nrow(dados))
treino <- dados[ran,] 
teste <- dados[-ran,] 

########################## KNN
### Faz um grid com valores para K e Executa o KNN
tuneGrid <- expand.grid(k = c(1))

knn <- train(tipo ~ ., data = treino, method = "knn",tuneGrid=tuneGrid)
knn

### Faz a predição e mostra a matriz de confusão
predict.knn <- predict(knn, teste)
confusionMatrix(predict.knn, as.factor(teste$tipo))

### PREDIÇÕES DE NOVOS CASOS

predict.knn <- predict(knn, dados_novos)
dados_novos$tipo <- NULL
dados_novos <- cbind(dados_novos, predict.knn)

########################## KNN
