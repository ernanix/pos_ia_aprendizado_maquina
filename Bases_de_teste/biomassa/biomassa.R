install.packages("caret") 
install.packages("e1071") 
install.packages("mlbench") 
install.packages("mice")
install.packages("Metrics")
library(mlbench) 
library(caret) 
library(mice)
library(Metrics)


dados <- read.csv(file = '/Users/MPPR/Documents/Pos_IA/pos_ia_aprendizado_maquina/Bases_de_teste/biomassa/biomassa.csv')
dados_novos <- read.csv(file = '/Users/MPPR/Documents/Pos_IA/pos_ia_aprendizado_maquina/Bases_de_teste/biomassa/biomassa_novos.csv')

###Set seed
set.seed(728078902)

### Cria arquivos de treino e teste
ind <- createDataPartition(dados$biomassa, p=0.80, list = FALSE)
treino <- dados[ind,]
teste <- dados[-ind,]

### Função R2
r2 <- function(predito, observado) {
  return(1 - (sum((predito-observado)^2) / sum((predito-mean(observado))^2)))
}


########################## KNN
### Prepara um grid com os valores de k que serão usados 
tuneGrid <- expand.grid(k = c(1,3,5,7,9))

### Executa o Knn com esse grid
knn <- train(biomassa ~ ., data = treino, method = "knn",
             tuneGrid=tuneGrid)

### Aplica o modelo no arquivo de teste
predict.knn <- predict(knn, teste)

### Mostra as métricas
rmse(teste$biomassa, predict.knn)
r2(predict.knn,teste$biomassa)

### PREDIÇÕES DE NOVOS CASOS
predict.knn <- predict(knn, dados_novos)
dados_novos$biomassa <- NULL
dados_novos <- cbind(dados_novos, predict.knn)

########################## KNN

########################## RNA
### Treino com Hold-Out
rna <- train(biomassa~., data=treino, method="nnet", linout=T, trace=FALSE)
predicoes.rna <- predict(rna, teste)

### Mostra as métricas
rmse(teste$biomassa, predicoes.rna)
r2(predicoes.rna, teste$biomassa)

### CV e parametrizacao da RNA
control <- trainControl(method = "cv", number = 10)
tuneGrid <- expand.grid(size = seq(from = 1, to = 3, by = 1), decay = seq(from = 0.1, to = 0.7, by = 0.3))
rna2 <- train(biomassa~., data=treino, method="nnet", trainControl=control, tuneGrid=tuneGrid, linout=T, MaxNWts=10000, maxit=2000, trace=F)

### Predições e métricas
predicoes.rna2 <- predict(rna2, teste)
rmse(teste$biomassa, predicoes.rna2)
r2(predicoes.rna2, teste$biomassa)

### Novos casos
predict.rna <- predict(rna, dados_novos)
dados_novos <- cbind(dados_novos, predict.rna)
predict.rna2 <- predict(rna2, dados_novos)
dados_novos <- cbind(dados_novos, predict.rna2)

########################## RNA

########################## SVN
### Treinar SVM com a base de Treino 
svm <- train(biomassa~., data=treino, method="svmRadial") 

### Aplicar modelos treinados na base de Teste
predicoes.svm <- predict(svm, teste)

### Calcular as métricas
rmse(teste$biomassa, predicoes.svm)
r2(predicoes.svm,teste$biomassa)

#### Cross-validation SVM
ctrl <- trainControl(method = "cv", number = 10)
svm2 <- train(biomassa~., data=treino, method="svmRadial", trControl=ctrl)

### 6. Aplicar modelos treinados na base de Teste
predicoes.svm2 <- predict(svm2, teste)

### Calcular as métricas
rmse(teste$biomassa, predicoes.svm2)
r2(predicoes.svm2 ,teste$biomassa)

#### Vários C e sigma
tuneGrid = expand.grid(C=c(1, 2, 10, 50, 100), sigma=c(.01, .015, 0.2))
svm3 <- train(biomassa~., data=treino, method="svmRadial", trControl=ctrl, tuneGrid=tuneGrid)

### 6. Aplicar modelos treinados na base de Teste
predicoes.svm3 <- predict(svm3, teste)
### Calcular as métricas
rmse(teste$biomassa, predicoes.svm3)
r2(predicoes.svm3,teste$biomassa)

### Novos casos
predict.svm <- predict(svm, dados_novos)
dados_novos <- cbind(dados_novos, predict.svm)
predict.svm2 <- predict(svm2, dados_novos)
dados_novos <- cbind(dados_novos, predict.svm2)
predict.svm3 <- predict(svm3, dados_novos)
dados_novos <- cbind(dados_novos, predict.svm3)

########################## SVN


