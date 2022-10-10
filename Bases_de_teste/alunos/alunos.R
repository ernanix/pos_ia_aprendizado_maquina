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

dados <- read.csv(file = paste('alunos','alunos.csv',sep =barra))
dados_novos <- read.csv(file = paste('alunos','alunos_novos.csv',sep =barra))

### Cria arquivos de treino e teste
set.seed(728078902)
ind <- createDataPartition(dados$G3, p=0.80, list = FALSE)
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
set.seed(728078902)
tuneGrid <- expand.grid(k = c(1,3,5,7,9))
knn <- train(G3 ~ ., data = treino, method = "knn",
             tuneGrid=tuneGrid)
knn
predict.knn <- predict(knn, teste)

### Mostra as métricas
F_r2(teste$G3,predict.knn)
F_SYX(teste$G3,predict.knn,teste)
F_PEARSON(teste$G3,predict.knn)
F_RMSE(teste$G3,predict.knn,teste)
F_MAE(teste$G3,predict.knn,teste)
########################## KNN

########################## RNA
set.seed(728078902)
rna <- train(G3~., data=treino, method="nnet", linout=T, trace=FALSE)
rna
predict.rna <- predict(rna, teste)

### Mostra as métricas
F_r2(teste$G3,predict.rna)
F_SYX(teste$G3,predict.rna,teste)
F_PEARSON(teste$G3,predict.rna)
F_RMSE(teste$G3,predict.rna,teste)
F_MAE(teste$G3,predict.rna,teste)

### CV
set.seed(728078902)
control <- trainControl(method = "cv", number = 10)
rna_cv <- train(G3~., data=treino, method="nnet", trainControl=control, linout=T, trace=F)
rna_cv
predict.rna_cv <- predict(rna_cv, teste)

F_r2(teste$G3,predict.rna_cv)
F_SYX(teste$G3,predict.rna_cv,teste)
F_PEARSON(teste$G3,predict.rna_cv)
F_RMSE(teste$G3,predict.rna_cv,teste)
F_MAE(teste$G3,predict.rna_cv,teste)

###Parametrização
set.seed(728078902)
tuneGrid <- expand.grid(size = seq(from = 1, to = 10, by = 1), decay = seq(from = 0.1, to = 0.9, by = 0.3))
rna_par <- train(G3~., data=treino, method="nnet", trainControl=control, tuneGrid=tuneGrid, linout=T, MaxNWts=10000, maxit=2000, trace=F)
rna_par
predict.rna_par <- predict(rna_par, teste)

F_r2(teste$G3,predict.rna_par)
F_SYX(teste$G3,predict.rna_par,teste)
F_PEARSON(teste$G3,predict.rna_par)
F_RMSE(teste$G3,predict.rna_par,teste)
F_MAE(teste$G3,predict.rna_par,teste)
########################## RNA

########################## SVN
set.seed(728078902)
svm <- train(G3~., data=treino, method="svmRadial") 
svm
predict.svm <- predict(svm, teste)

F_r2(teste$G3,predict.svm)
F_SYX(teste$G3,predict.svm,teste)
F_PEARSON(teste$G3,predict.svm)
F_RMSE(teste$G3,predict.svm,teste)
F_MAE(teste$G3,predict.svm,teste)

#### Cross-validation SVM
set.seed(728078902)
ctrl <- trainControl(method = "cv", number = 10)
svm_cv <- train(G3~., data=treino, method="svmRadial", trControl=ctrl)
svm_cv
predict.svm_cv <- predict(svm_cv, teste)

F_r2(teste$G3,predict.svm_cv)
F_SYX(teste$G3,predict.svm_cv,teste)
F_PEARSON(teste$G3,predict.svm_cv)
F_RMSE(teste$G3,predict.svm_cv,teste)
F_MAE(teste$G3,predict.svm_cv,teste)

#### Parametrização
set.seed(728078902)
tuneGrid = expand.grid(C=c(1, 2, 10, 50, 100), sigma=c(.01, .015, 0.2))
svm_par <- train(G3~., data=treino, method="svmRadial", tuneGrid=tuneGrid)
svm_par
predict.svm_par <- predict(svm_par, teste)

F_r2(teste$G3,predict.svm_par)
F_SYX(teste$G3,predict.svm_par,teste)
F_PEARSON(teste$G3,predict.svm_par)
F_RMSE(teste$G3,predict.svm_par,teste)
F_MAE(teste$G3,predict.svm_par,teste)

########################## SVN

########################## Random Forest
set.seed(728078902)
rf <- train(G3~.,data=treino,method="rf")
rf
predict.rf <- predict(rf,teste)

F_r2(teste$G3,predict.rf)
F_SYX(teste$G3,predict.rf,teste)
F_PEARSON(teste$G3,predict.rf)
F_RMSE(teste$G3,predict.rf,teste)
F_MAE(teste$G3,predict.rf,teste)

##Cross Validation
set.seed(728078902)
ctrl <- trainControl(method="cv",number = 10)
rf_cv <- train(G3~.,data=treino,method="rf",trControl=ctrl)
rf_cv
predict.rf_cv <- predict(rf_cv,teste)

F_r2(teste$G3,predict.rf_cv)
F_SYX(teste$G3,predict.rf_cv,teste)
F_PEARSON(teste$G3,predict.rf_cv)
F_RMSE(teste$G3,predict.rf_cv,teste)
F_MAE(teste$G3,predict.rf_cv,teste)

##Varios mtry
set.seed(728078902)
tuneGrid = expand.grid(mtry=c(21,31,41,43,45))
rf_par <- train(G3~.,data=treino,method="rf",trControl=ctrl,tuneGrid=tuneGrid)
rf_par
predict.rf_par <- predict(rf_par,teste)

F_r2(teste$G3,predict.rf_par)
F_SYX(teste$G3,predict.rf_par,teste)
F_PEARSON(teste$G3,predict.rf_par)
F_RMSE(teste$G3,predict.rf_par,teste)
F_MAE(teste$G3,predict.rf_par,teste)

predict.rf <- predict(rf, dados_novos)
dados_novos <- cbind(dados_novos, predict.rf)
predict.rf2 <- predict(rf2, dados_novos)
dados_novos <- cbind(dados_novos, predict.rf2)
predict.rf3 <- predict(rf3, dados_novos)
dados_novos <- cbind(dados_novos, predict.rf3)
########################## Random Forest

########################## Novos Casos
dados_novos$G3 <- NULL
predict.melhor_modelo <- predict(rf_cv,dados_novos)
dados_novos <- cbind(dados_novos, predict.melhor_modelo)
View(dados_novos)

########################## Novos Casos

########################## Gráfico de Resíduos

resid = ((teste$G3 - predict.rf_cv)/teste$G3) * 100

plot(resid ~ predict.rf_cv,
     xlab="Valor estimado",
     ylab="Resíduos (%)",
     col=2)
abline(h=0,col=4)

########################## Gráfico de Resíduos

