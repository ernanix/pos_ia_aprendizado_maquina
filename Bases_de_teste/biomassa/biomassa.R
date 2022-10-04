install.packages("caret") 
install.packages("e1071") 
install.packages("mlbench") 
install.packages("mice")
install.packages("Metrics")
library(mlbench) 
library(caret) 
library(mice)
library(Metrics)

##Maquina MP
setwd('C:\\Users\\escneto\\Documents\\Estudos\\Pos_IA_UFPR\\pos_ia_aprendizado_maquina\\Bases_de_teste')
barra ="\\"
##Note
setwd('/Users/MPPR/Documents/Pos_IA/pos_ia_aprendizado_maquina/Bases_de_teste')
barra ="/"

dados <- read.csv(file = paste('biomassa','biomassa.csv',sep =barra))
dados_novos <- read.csv(file = paste('biomassa','biomassa_novos.csv',sep =barra))

imp <- mice(dados)
dados <- complete(imp,1)

###Set seed
set.seed(728078902)

### Cria arquivos de treino e teste
ind <- createDataPartition(dados$biomassa, p=0.80, list = FALSE)
treino <- dados[ind,]
teste <- dados[-ind,]

### Função R2
F_r2 <- function(observado,predito) {
  return (1 - (sum((predito-observado)^2) / sum((predito-mean(observado))^2)))
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
  val1 = sum((observado-predito)^2)
  val2 = nrow(base) - (length(base)-1)
  return (sqrt(val1 / val2))
}
### Função Pearson
F_PEARSON <- function(observado,predito) {
  val1 = sum((observado-mean(observado)) * (predito-mean(predito)))
  val2 = sqrt(sum((observado-mean(observado))^2))
  val3 = sqrt(sum((predito-mean(predito))^2))
  return (val1 / (val2 * val3))
}


########################## KNN
### Prepara um grid com os valores de k que serão usados 
tuneGrid <- expand.grid(k = c(1,3,5,7,9))

### Executa o Knn com esse grid
knn <- train(biomassa ~ ., data = treino, method = "knn",
             tuneGrid=tuneGrid)
knn
### Aplica o modelo no arquivo de teste
predict.knn <- predict(knn, teste)

### Mostra as métricas
F_r2(teste$biomassa,predict.knn)
F_SYX(teste$biomassa,predict.knn,teste)
F_PEARSON(teste$biomassa,predict.knn)
F_RMSE(teste$biomassa,predict.knn,teste)
F_MAE(teste$biomassa,predict.knn,teste)


### PREDIÇÕES DE NOVOS CASOS
predict.knn <- predict(knn, dados_novos)
dados_novos$biomassa <- NULL
dados_novos <- cbind(dados_novos, predict.knn)

########################## KNN

########################## RNA
### Treino com Hold-Out
rna <- train(biomassa~., data=treino, method="nnet", linout=T, trace=FALSE)
rna

predict.rna <- predict(rna, teste)

### Mostra as métricas
F_r2(teste$biomassa,predict.rna)
F_SYX(teste$biomassa,predict.rna,teste)
F_PEARSON(teste$biomassa,predict.rna)
F_RMSE(teste$biomassa,predict.rna,teste)
F_MAE(teste$biomassa,predict.rna,teste)

### CV
control <- trainControl(method = "cv", number = 10)
rna_cv <- train(biomassa~., data=treino, method="nnet", trainControl=control, linout=T, trace=F)
rna_cv
predict.rna_cv <- predict(rna_cv, teste)
F_r2(teste$biomassa,predict.rna_cv)
F_SYX(teste$biomassa,predict.rna_cv,teste)
F_PEARSON(teste$biomassa,predict.rna_cv)
F_RMSE(teste$biomassa,predict.rna_cv,teste)
F_MAE(teste$biomassa,predict.rna_cv,teste)

###Parametrização
tuneGrid <- expand.grid(size = seq(from = 1, to = 10, by = 1), decay = seq(from = 0.1, to = 0.9, by = 0.3))
rna_par <- train(biomassa~., data=treino, method="nnet", trainControl=control, tuneGrid=tuneGrid, linout=T, MaxNWts=10000, maxit=2000, trace=F)
rna_par
predict.rna_par <- predict(rna_par, teste)
F_r2(teste$biomassa,predict.rna_par)
F_SYX(teste$biomassa,predict.rna_par,teste)
F_PEARSON(teste$biomassa,predict.rna_par)
F_RMSE(teste$biomassa,predict.rna_par,teste)
F_MAE(teste$biomassa,predict.rna_par,teste)

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


