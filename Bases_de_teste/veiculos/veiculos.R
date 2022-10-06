library("caret")
library(mlbench)
library(mice)

##Maquina MP
setwd('C:\\Users\\escneto\\Documents\\Estudos\\Pos_IA_UFPR\\pos_ia_aprendizado_maquina\\Bases_de_teste')
barra ="\\"
##Note
setwd('/Users/MPPR/Documents/Pos_IA/pos_ia_aprendizado_maquina/Bases_de_teste')
barra ="/"

dados <- read.csv(file = paste('veiculos','veiculos.csv',sep =barra))
dados_novos <- read.csv(file = paste('veiculos','veiculos_novos.csv',sep =barra))

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

########################## KNN

########################## RNA
imp <- mice(dados)
dados <- complete(imp,1)

rna <- train(tipo~.,data=treino,method="nnet",trace=FALSE)
rna

### Faz a predição e mostra a matriz de confusão
predict.rna <-predict(rna,teste)
confusionMatrix(predict.rna,as.factor(teste$tipo))

###Cross Validation
ctrl <- trainControl(method = "cv", number = 10)
rna_cv <- train(tipo~.,data=treino,method="nnet",trace=FALSE,trControl=ctrl)
rna_cv
predict.rna_cv <- predict(rna_cv,teste)
confusionMatrix(predict.rna_cv,as.factor(teste$tipo))


###Parametrização
grid <-expand.grid(size=seq(from=1,to=45,by=10),decay=seq(from=0.1, to=0.9, by=0.3))
rna_par <- train(form=tipo~., data=treino, method="nnet", tuneGrid=grid, trControl=ctrl, maxit=2000, trace=FALSE)
rna_par

predict.rna_par <- predict(rna_par,teste)
confusionMatrix(predict.rna_par,as.factor(teste$tipo))
########################## RNA

########################## SVM
svm <- train(tipo~.,data=treino,method="svmRadial")
svm
predict.svm <- predict(svm,teste)
confusionMatrix(predict.svm,as.factor(teste$tipo))

###Cross Validation
ctrl <- trainControl(method="cv",number=10)
svm_cv <- train(tipo~.,data=treino,method="svmRadial",trControl=ctrl)
svm_cv
predict.svm_cv <- predict(svm_cv,teste)
confusionMatrix(predict.svm_cv,as.factor(teste$tipo))

###Parametrização
tuneGrid = expand.grid(C=c(1,2,10,50,100),sigma=c(.01,.015,0.2))
svm_par <- train(tipo~.,data=treino,method="svmRadial",trControl=ctrl,tuneGrid=tuneGrid)
svm_par
predict.svm_par <- predict(svm_par,teste)
confusionMatrix(predict.svm_par,as.factor(teste$tipo))
########################## SVM