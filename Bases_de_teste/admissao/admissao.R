library("caret")
library(Metrics)

##Maquina MP
setwd('C:\\Users\\escneto\\Documents\\Estudos\\Pos_IA_UFPR\\pos_ia_aprendizado_maquina\\Bases_de_teste')


dados <- read.csv(file = 'admissao\\admissao.csv')
dados_novos <- read.csv(file = 'admissao\\admissao.csv')

set.seed(728078902)
### Cria arquivos de treino e teste
ind <- createDataPartition(dados$ChanceOfAdmit, p=0.80, list = FALSE)
treino <- dados[ind,]
teste <- dados[-ind,]

### Função R2
F_r2 <- function(observado,predito) {
  return(1 - (sum((observado-predito)^2) / sum((observado-mean(observado))^2)))
}
### Função MAE
F_MAE <- function(observado,predito,base) {
  return(sum(abs(observado-predito)) / nrow(base)) 
}
### Função RMSE
F_RMSE <- function(observado,predito,base) {
  return( sqrt(sum((observado-predito)^2) / nrow(base)) ) 
}
### Função Syx
F_SYX <- function(observado,predito,base) {
  return(sum((observado-predito)^2) / (nrow(base) - (length(base)-1) ) )
}

########################## KNN
tuneGrid <- expand.grid(k = c(1,3,5,7,9))
### Executa o Knn com esse grid
knn <- train(ChanceOfAdmit ~ ., data = treino, method = "knn",
             tuneGrid=tuneGrid)
### Aplica o modelo no arquivo de teste
predict.knn <- predict(knn, teste)
### Mostra as métricas
rmse(teste$ChanceOfAdmit, predict.knn)
r2(teste$ChanceOfAdmit,predict.knn)
MAE(predict.knn,teste$ChanceOfAdmit)

### Novos casos
predict.knn <- predict(knn, dados_novos)
dados_novos <- cbind(dados_novos, predict.knn)
########################## KNN

########################## RNA
### Treino com Hold-Out
rna <- train(ChanceOfAdmit~., data=treino, method="nnet", linout=T, trace=FALSE)
predicoes.rna <- predict(rna, teste)

### Mostra as métricas
rmse(teste$ChanceOfAdmit, predicoes.rna)
r2(predicoes.rna, teste$ChanceOfAdmit)

### CV e parametrizacao da RNA
control <- trainControl(method = "cv", number = 10)
tuneGrid <- expand.grid(size = seq(from = 1, to = 3, by = 1), decay = seq(from = 0.1, to = 0.7, by = 0.3))
rna2 <- train(ChanceOfAdmit~., data=treino, method="nnet", trainControl=control, tuneGrid=tuneGrid, linout=T, MaxNWts=10000, maxit=2000, trace=F)

### Predições e métricas
predicoes.rna2 <- predict(rna2, teste)
rmse(teste$ChanceOfAdmit, predicoes.rna2)
r2(predicoes.rna2, teste$ChanceOfAdmit)

### Novos casos
predict.rna <- predict(rna, dados_novos)
dados_novos <- cbind(dados_novos, predict.rna)
predict.rna2 <- predict(rna2, dados_novos)
dados_novos <- cbind(dados_novos, predict.rna2)

########################## RNA

########################## SVN
### Treinar SVM com a base de Treino 
svm <- train(ChanceOfAdmit~., data=treino, method="svmRadial") 

### Aplicar modelos treinados na base de Teste
predicoes.svm <- predict(svm, teste)

### Calcular as métricas
rmse(teste$ChanceOfAdmit, predicoes.svm)
r2(predicoes.svm,teste$ChanceOfAdmit)

#### Cross-validation SVM
ctrl <- trainControl(method = "cv", number = 10)
svm2 <- train(ChanceOfAdmit~., data=treino, method="svmRadial", trControl=ctrl)

###Aplicar modelos treinados na base de Teste
predicoes.svm2 <- predict(svm2, teste)

### Calcular as métricas
rmse(teste$ChanceOfAdmit, predicoes.svm2)
r2(predicoes.svm2 ,teste$ChanceOfAdmit)

#### Vários C e sigma
tuneGrid = expand.grid(C=c(1, 2, 10, 50, 100), sigma=c(.01, .015, 0.2))
svm3 <- train(ChanceOfAdmit~., data=treino, method="svmRadial", trControl=ctrl, tuneGrid=tuneGrid)

### 6. Aplicar modelos treinados na base de Teste
predicoes.svm3 <- predict(svm3, teste)
### Calcular as métricas
rmse(teste$ChanceOfAdmit, predicoes.svm3)
r2(predicoes.svm3,teste$ChanceOfAdmit)

### Novos casos
predict.svm <- predict(svm, dados_novos)
dados_novos <- cbind(dados_novos, predict.svm)
predict.svm2 <- predict(svm2, dados_novos)
dados_novos <- cbind(dados_novos, predict.svm2)
predict.svm3 <- predict(svm3, dados_novos)
dados_novos <- cbind(dados_novos, predict.svm3)

########################## SVN

########################## Random Forest

rf <- train(ChanceOfAdmit~.,data=treino,method="rf")
predicoes.rf <- predict(rf,teste)

rmse(teste$ChanceOfAdmit,predicoes.rf)
r2(predicoes.rf,teste$ChanceOfAdmit)

##Cross Validation
ctrl <- trainControl(method="cv",number = 10)

rf2 <- train(ChanceOfAdmit~.,data=treino,method="rf",trControl=ctrl)
predicoes.rf2 <- predict(rf2,teste)

rmse(teste$ChanceOfAdmit,predicoes.rf2)
r2(predicoes.rf2,teste$ChanceOfAdmit)

##Varios mtry
tuneGrid = expand.grid(mtry=c(2,5,7,9))

rf3 <- train(ChanceOfAdmit~.,data=treino,method="rf",trControl=ctrl,tuneGrid=tuneGrid)
predicoes.rf3 <- predict(rf3,teste)
rmse(teste$ChanceOfAdmit,predicoes.rf3)
r2(predicoes.rf3,teste$ChanceOfAdmit)

predict.rf <- predict(rf, dados_novos)
dados_novos <- cbind(dados_novos, predict.rf)
predict.rf2 <- predict(rf2, dados_novos)
dados_novos <- cbind(dados_novos, predict.rf2)
predict.rf3 <- predict(rf3, dados_novos)
dados_novos <- cbind(dados_novos, predict.rf3)
########################## Random Forest
