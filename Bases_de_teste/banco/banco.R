library("caret")
library(mice)

dados <- read.csv(file = '/Users/MPPR/Documents/Pos_IA/pos_ia_aprendizado_maquina/Bases_de_teste/banco/banco.csv')
dados_novos <- read.csv(file = '/Users/MPPR/Documents/Pos_IA/pos_ia_aprendizado_maquina/Bases_de_teste/banco/banco_novos.csv')

### Set Seed
set.seed(728078902)

### Cria arquivos de treino e teste
ran <- sample(1:nrow(dados), 0.8 * nrow(dados))
treino <- dados[ran,] 
teste <- dados[-ran,] 

########################## KNN
### Faz um grid com valores para K e Executa o KNN
tuneGrid <- expand.grid(k = c(1,3,5,7,9))
knn <- train(y~., data = treino, method = "knn",tuneGrid=tuneGrid)

### Faz a predição e mostra a matriz de confusão
predict.knn <- predict(knn, teste)
confusionMatrix(predict.knn, as.factor(teste$y))

### PREDIÇÕES DE NOVOS CASOS
predict.knn <- predict(knn, dados_novos)
dados_novos$y <- NULL
dados_novos <- cbind(dados_novos, predict.knn)


########################## KNN

########################## RNA
########## Treinar o modelo com Hold-out
rna <- train(y~., data=treino, method="nnet",trace=FALSE)
### Predições dos valores do conjunto de teste
predict.rna <- predict(rna, teste)
### Matriz de confusão
confusionMatrix(predict.rna, as.factor(teste$y))

########## Usando Cross-validation
### indica o método cv e numero de folders 10
ctrl <- trainControl(method = "cv", number = 10)
### executa a RNA com esse ctrl
rna2 <- train(y~., data=treino, method="nnet",trace=FALSE, trControl=ctrl)
predict.rna2 <- predict(rna2, teste) 
confusionMatrix(predict.rna2, as.factor(teste$y))

########### Parametrização da RNA
### size, decay
grid <- expand.grid(size = seq(from = 1, to = 35, by = 10),decay = seq(from = 0.1, to = 0.6, by = 0.3))

rna3 <- train(
  form = y~. , 
  data = treino , 
  method = "nnet" , 
  tuneGrid = grid , 
  trControl = ctrl , 
  maxit = 2000,trace=FALSE) 
### Faz as predições e mostra matriz de confusão
predict.rna3 <- predict(rna3, teste)
confusionMatrix(predict.rna3, as.factor(teste$y))

### PREDIÇÕES DE NOVOS CASOS
predict.rna <- predict(rna, dados_novos)
dados_novos <- cbind(dados_novos, predict.rna)
predict.rna2 <- predict(rna2, dados_novos)
dados_novos <- cbind(dados_novos, predict.rna2)
predict.rna3 <- predict(rna3, dados_novos)
dados_novos <- cbind(dados_novos, predict.rna3)
########################## RNA

########################## SVN
### Treinar SVM com a base de Treino 
svm <- train(y~., data=treino, method="svmRadial") 
### 6. Aplicar modelos treinados na base de Teste
predict.svm <- predict(svm, teste)
confusionMatrix(predict.svm, as.factor(teste$y))

#### Cross-validation SVM
ctrl <- trainControl(method = "cv", number = 10)
svm2 <- train(y~., data=treino, method="svmRadial", trControl=ctrl)
### matriz de confusao com todos os dados
predict.svm2 <- predict(svm2, teste)
confusionMatrix(predict.svm2, as.factor(teste$y))

#### Vários C e sigma
tuneGrid = expand.grid(C=c(1, 2, 10, 50, 100), sigma=c(.01, .015, 0.2))
svm3 <- train(y~., data=treino, method="svmRadial", trControl=ctrl, tuneGrid=tuneGrid)
### matriz de confusao com todos os dados
predict.svm3 <- predict(svm3, teste)
confusionMatrix(predict.svm3, as.factor(teste$y))

### PREDIÇÕES DE NOVOS CASOS
predict.svm <- predict(svm, dados_novos)
dados_novos <- cbind(dados_novos, predict.svm)
predict.svm2 <- predict(svm2, dados_novos)
dados_novos <- cbind(dados_novos, predict.svm2)
predict.svm3 <- predict(svm3, dados_novos)
dados_novos <- cbind(dados_novos, predict.svm3)
########################## SVN

########################## Random Forest
rf <- train(y~.,data=treino,method="rf")
predict.rf <-predict(rf,teste)
confusionMatrix(predict.rf,as.factor(teste$y))

##Cross Validation
ctrl <- trainControl(method = "cv", number = 10)
rf2 <- train(y~.,data=treino,method="rf",trControl=ctrl)
predict.rf2 <-predict(rf2,teste)
confusionMatrix(predict.rf2,as.factor(teste$y))

##Vários mtry
tuneGrid = expand.grid(mtry=c(2,5,7,9))
rf3 <- train(y~.,data=treino,method="rf",trControl=ctrl,tuneGrid=tuneGrid)
predict.rf3 <- predict(rf3,teste)
confusionMatrix(predict.rf3,as.factor(teste$y))

##novos dados
pred.rf <- predict(rf,dados_novos)
dados_novos <- cbind(dados_novos,pred.rf)
pred.rf2 <- predict(rf2,dados_novos)
dados_novos <- cbind(dados_novos,pred.rf2)
pred.rf3 <- predict(rf3,dados_novos)
dados_novos <- cbind(dados_novos,pred.rf3)

########################## Random Forest

