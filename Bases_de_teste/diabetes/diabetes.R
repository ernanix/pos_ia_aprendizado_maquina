library("caret")
library(mlbench)
library(mice)

##Maquina MP
setwd('C:\\Users\\escneto\\Documents\\Estudos\\Pos_IA_UFPR\\pos_ia_aprendizado_maquina\\Bases_de_teste')
barra ="\\"
##Note
setwd('/Users/MPPR/Documents/Pos_IA/pos_ia_aprendizado_maquina/Bases_de_teste')
barra ="/"

dados <- read.csv(file = paste('diabetes','diabetes.csv',sep =barra))
dados_novos <- read.csv(file = paste('diabetes','diabetes_novos.csv',sep =barra))


### Set Seed
set.seed(728078902)

### Cria arquivos de treino e teste
ran <- sample(1:nrow(dados), 0.8 * nrow(dados))
treino <- dados[ran,] 
teste <- dados[-ran,] 

########################## KNN
### Faz um grid com valores para K e Executa o KNN
tuneGrid <- expand.grid(k = c(9))
knn <- train(diabetes~., data = treino, method = "knn",tuneGrid=tuneGrid)

### Faz a predição e mostra a matriz de confusão
predict.knn <- predict(knn, teste)
confusionMatrix(predict.knn, as.factor(teste$diabetes))

### PREDIÇÕES DE NOVOS CASOS
predict.knn <- predict(knn, dados_novos)
dados_novos$diabetes <- NULL
dados_novos <- cbind(dados_novos, predict.knn)

########################## KNN

########################## RNA
imp <- mice(dados)
dados <- complete(imp,1)

########## Treinar o modelo com Hold-out
rna <- train(diabetes~., data=treino, method="nnet",trace=FALSE)
rna
### Predições dos valores do conjunto de teste
predict.rna <- predict(rna, teste)
### Matriz de confusão
confusionMatrix(predict.rna, as.factor(teste$diabetes))

########## Usando Cross-validation
### indica o método cv e numero de folders 10
ctrl <- trainControl(method = "cv", number = 10)
### executa a RNA com esse ctrl
rna_cv <- train(diabetes~., data=treino, method="nnet",trace=FALSE, trControl=ctrl)
rna_cv
predict.rna_cv <- predict(rna_cv, teste) 
confusionMatrix(predict.rna_cv, as.factor(teste$diabetes))

########### Parametrização da RNA
### size, decay
grid <- expand.grid(size = seq(from = 1, to = 45, by = 10),decay = seq(from = 0.1, to = 0.9, by = 0.3))

rna_par <- train(
  form = diabetes~. , 
  data = treino , 
  method = "nnet" , 
  tuneGrid = grid , 
  trControl = ctrl , 
  maxit = 2000,trace=FALSE) 

rna_par
### Faz as predições e mostra matriz de confusão
predict.rna_par <- predict(rna_par, teste)
confusionMatrix(predict.rna_par, as.factor(teste$diabetes))

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
svm <- train(diabetes~., data=treino, method="svmRadial") 
### 6. Aplicar modelos treinados na base de Teste
predict.svm <- predict(svm, teste)
confusionMatrix(predict.svm, as.factor(teste$diabetes))

#### Cross-validation SVM
ctrl <- trainControl(method = "cv", number = 10)
svm2 <- train(diabetes~., data=treino, method="svmRadial", trControl=ctrl)
### matriz de confusao com todos os dados
predict.svm2 <- predict(svm2, teste)
confusionMatrix(predict.svm2, as.factor(teste$diabetes))

#### Vários C e sigma
tuneGrid = expand.grid(C=c(1, 2, 10, 50, 100), sigma=c(.01, .015, 0.2))
svm3 <- train(diabetes~., data=treino, method="svmRadial", trControl=ctrl, tuneGrid=tuneGrid)
### matriz de confusao com todos os dados
predict.svm3 <- predict(svm3, teste)
confusionMatrix(predict.svm3, as.factor(teste$diabetes))

### PREDIÇÕES DE NOVOS CASOS
predict.svm <- predict(svm, dados_novos)
dados_novos <- cbind(dados_novos, predict.svm)
predict.svm2 <- predict(svm2, dados_novos)
dados_novos <- cbind(dados_novos, predict.svm2)
predict.svm3 <- predict(svm3, dados_novos)
dados_novos <- cbind(dados_novos, predict.svm3)
