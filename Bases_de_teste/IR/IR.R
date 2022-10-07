library("caret")
library(mlbench)
library(mice)

##Maquina MP
setwd('C:\\Users\\escneto\\Documents\\Estudos\\Pos_IA_UFPR\\pos_ia_aprendizado_maquina\\Bases_de_teste')
barra ="\\"
##Note
setwd('/Users/MPPR/Documents/Pos_IA/pos_ia_aprendizado_maquina/Bases_de_teste')
barra ="/"

dados <- read.csv(file = paste('IR','IR.csv',sep =barra))
dados_novos <- read.csv(file = paste('IR','IR_novos.csv',sep =barra))

### Cria arquivos de treino e teste
set.seed(728078902)
ran <- sample(1:nrow(dados), 0.8 * nrow(dados))
treino <- dados[ran,] 
teste <- dados[-ran,] 

########################## KNN
set.seed(728078902)
tuneGrid <- expand.grid(k = c(1,3,5,7,9))
knn <- train(sonegador~., data = treino, method = "knn",tuneGrid=tuneGrid)
knn
predict.knn <- predict(knn, teste)
confusionMatrix(predict.knn, as.factor(teste$sonegador))
########################## KNN

########################## RNA
###Hold-out
set.seed(728078902)
rna <- train(sonegador~.,data=treino,method="nnet",trace=FALSE)
rna
predict.rna <-predict(rna,teste)
confusionMatrix(predict.rna,as.factor(teste$sonegador))

###Cross Validation
set.seed(728078902)
ctrl <- trainControl(method = "cv", number = 10)
rna_cv <- train(sonegador~.,data=treino,method="nnet",trace=FALSE,trControl=ctrl)
rna_cv
predict.rna_cv <- predict(rna_cv,teste)
confusionMatrix(predict.rna_cv,as.factor(teste$sonegador))

###Parametrização
set.seed(728078902)
grid <-expand.grid(size=seq(from=1,to=45,by=10),decay=seq(from=0.1, to=0.9, by=0.3))
rna_par <- train(form=sonegador~., data=treino, method="nnet", tuneGrid=grid, trControl=ctrl, maxit=2000, trace=FALSE)
rna_par
predict.rna_par <- predict(rna_par,teste)
confusionMatrix(predict.rna_par,as.factor(teste$sonegador))
########################## RNA

########################## SVM
###Hold-out
set.seed(728078902)
svm <- train(sonegador~.,data=treino,method="svmRadial")
svm
predict.svm <- predict(svm,teste)
confusionMatrix(predict.svm,as.factor(teste$sonegador))

###Cross Validation
set.seed(728078902)
ctrl <- trainControl(method="cv",number=10)
svm_cv <- train(sonegador~.,data=treino,method="svmRadial",trControl=ctrl)
svm_cv
predict.svm_cv <- predict(svm_cv,teste)
confusionMatrix(predict.svm_cv,as.factor(teste$sonegador))

###Parametrização
set.seed(728078902)
tuneGrid = expand.grid(C=c(1,2,10,50,100),sigma=c(.01,.015,0.2))
svm_par <- train(sonegador~.,data=treino,method="svmRadial",trControl=ctrl,tuneGrid=tuneGrid)
svm_par
predict.svm_par <- predict(svm_par,teste)
confusionMatrix(predict.svm_par,as.factor(teste$sonegador))
########################## SVM

########################## Random Forest
set.seed(728078902)
rf <- train(sonegador~.,data=treino,method="rf")
rf
predict.rf <- predict(rf,teste)
confusionMatrix(predict.rf,as.factor(teste$sonegador))

###Cross Validation
set.seed(728078902)
ctrl <- trainControl(method="cv",number=10)
rf_cv <- train(sonegador~.,data=treino,method="rf",trControl=ctrl)
rf_cv
predict.rf_cv <- predict(rf_cv,teste)
confusionMatrix(predict.rf_cv,as.factor(teste$sonegador))

###Parametrização
set.seed(728078902)
tuneGrid = expand.grid(mtry=c(2, 5, 7, 9))
rf_par <- train(sonegador~.,data=treino,method="rf",trControl=ctrl,tuneGrid=tuneGrid)
rf_par
predict.rf_par <- predict(rf_par,teste)
confusionMatrix(predict.rf_par,as.factor(teste$sonegador))

########################## Random Forest

########################## Novos casos
dados_novos$sonegador <-NULL
predict.melhor_modelo <- predict(rna,dados_novos)
dados_novos <-cbind(dados_novos,predict.melhor_modelo)
View(dados_novos)
########################## Novos casos
