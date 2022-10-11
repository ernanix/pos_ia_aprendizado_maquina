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
dados$num <- NULL
dados_novos$num <-NULL

### Cria arquivos de treino e teste
set.seed(728078902)
ran <- sample(1:nrow(dados), 0.8 * nrow(dados))
treino <- dados[ran,] 
teste <- dados[-ran,] 

########################## KNN
set.seed(728078902)
tuneGrid <- expand.grid(k = c(1,3,5,7,9))
knn <- train(diabetes~., data = treino, method = "knn",tuneGrid=tuneGrid)
knn
predict.knn <- predict(knn, teste)
confusionMatrix(predict.knn, as.factor(teste$diabetes))
########################## KNN

########################## RNA
###Hold-out
set.seed(728078902)
rna <- train(diabetes~., data=treino, method="nnet",trace=FALSE)
rna
predict.rna <- predict(rna, teste)
confusionMatrix(predict.rna, as.factor(teste$diabetes))

###Cross-validation
set.seed(728078902)
ctrl <- trainControl(method = "cv", number = 10)
rna_cv <- train(diabetes~., data=treino, method="nnet",trace=FALSE, trControl=ctrl)
rna_cv
predict.rna_cv <- predict(rna_cv, teste) 
confusionMatrix(predict.rna_cv, as.factor(teste$diabetes))

########### Parametrização
set.seed(728078902)
grid <- expand.grid(size = seq(from = 1, to = 45, by = 10),decay = seq(from = 0.1, to = 0.9, by = 0.3))
rna_par <- train(form = diabetes~.,data = treino,method = "nnet",tuneGrid = grid,trControl = ctrl,maxit = 2000,trace=FALSE) 
rna_par
predict.rna_par <- predict(rna_par, teste)
confusionMatrix(predict.rna_par, as.factor(teste$diabetes))
########################## RNA

########################## SVN
###Hold-out 
set.seed(728078902)
svm <- train(diabetes~., data=treino, method="svmRadial") 
svm
predict.svm <- predict(svm, teste)
confusionMatrix(predict.svm, as.factor(teste$diabetes))

#### Cross-validation
set.seed(728078902)
ctrl <- trainControl(method = "cv", number = 10)
svm_cv <- train(diabetes~., data=treino, method="svmRadial", trControl=ctrl)
svm_cv
predict.svm_cv <- predict(svm_cv, teste)
confusionMatrix(predict.svm_cv, as.factor(teste$diabetes))

#### Parametrização
set.seed(728078902)
tuneGrid = expand.grid(C=c(1, 2, 10, 50, 100), sigma=c(.01, .015, 0.2))
svm_par <- train(diabetes~., data=treino, method="svmRadial", trControl=ctrl, tuneGrid=tuneGrid)
svm_par
predict.svm_par <- predict(svm_par, teste)
confusionMatrix(predict.svm_par, as.factor(teste$diabetes))

########################## Random Forest
set.seed(728078902)
rf <- train(diabetes~.,data=treino,method="rf")
rf
predict.rf <- predict(rf,teste)
confusionMatrix(predict.rf,as.factor(teste$diabetes))

###Cross Validation
set.seed(728078902)
ctrl <- trainControl(method="cv",number=10)
rf_cv <- train(diabetes~.,data=treino,method="rf",trControl=ctrl)
rf_cv
predict.rf_cv <- predict(rf_cv,teste)
confusionMatrix(predict.rf_cv,as.factor(teste$diabetes))

###Parametrização
set.seed(728078902)
tuneGrid = expand.grid(mtry=c(2, 5, 7, 9))
rf_par <- train(diabetes~.,data=treino,method="rf",trControl=ctrl,tuneGrid=tuneGrid)
rf_par
predict.rf_par <- predict(rf_par,teste)
confusionMatrix(predict.rf_par,as.factor(teste$diabetes))

########################## Random Forest

########################## Novos casos
dados_novos$diabetes <-NULL
predict.melhor_modelo <- predict(svm_par,dados_novos)
dados_novos <-cbind(dados_novos,predict.melhor_modelo)
View(dados_novos)
########################## Novos casos

########################## Analise ROC
cmknn <- confusionMatrix(predict.knn,as.factor(teste$diabetes))
cmrna <- confusionMatrix(predict.rna,as.factor(teste$diabetes))
cmrna_cv <- confusionMatrix(predict.rna_cv,as.factor(teste$diabetes))
cmrna_par <- confusionMatrix(predict.rna_par,as.factor(teste$diabetes))
cmsvm <- confusionMatrix(predict.svm,as.factor(teste$diabetes))
cmsvm_cv <- confusionMatrix(predict.svm_cv,as.factor(teste$diabetes))
cmsvm_par <- confusionMatrix(predict.svm_par,as.factor(teste$diabetes))
cmrf <- confusionMatrix(predict.rf,as.factor(teste$diabetes))
cmrf_cv <- confusionMatrix(predict.rf_cv,as.factor(teste$diabetes))
cmrf_par <- confusionMatrix(predict.rf_par,as.factor(teste$diabetes))



####Funçao
df.ROC <- data.frame(
        modelo = c('knn','rna','rna_cv','rna_par','svm','svm_cv','svm_par','rf','rf_cv','rf_par'),
        X = c(rep(0,10)),
        Y = c(rep(0,10)),
        distancia = c(rep(0,10))
  )

func_ROC <- function(df,cm,modelo) {
  VP <- cm[1,1]
  FP <- cm[2,1]
  VN <- cm[2,2]
  FN <- cm[1,2]
  
  X <- 1-(VN / (VN+FP))
  Y <- VP / (VP+FN)
  distancia <- sqrt(X^2 + (Y-1)^2)
  
  df[df$modelo==modelo,"X"] <- X
  df[df$modelo==modelo,"Y"] <- Y
  df[df$modelo==modelo,"distancia"] <- distancia
  
  return (df)
}

df.ROC <- func_ROC(df.ROC,cmknn$table,"knn")
df.ROC <- func_ROC(df.ROC,cmrna$table,"rna")
df.ROC <- func_ROC(df.ROC,cmrna_cv$table,"rna_cv")
df.ROC <- func_ROC(df.ROC,cmrna_par$table,"rna_par")
df.ROC <- func_ROC(df.ROC,cmsvm$table,"svm")
df.ROC <- func_ROC(df.ROC,cmsvm_cv$table,"svm_cv")
df.ROC <- func_ROC(df.ROC,cmsvm_par$table,"svm_par")
df.ROC <- func_ROC(df.ROC,cmrf$table,"rf")
df.ROC <- func_ROC(df.ROC,cmrf_cv$table,"rf_cv")
df.ROC <- func_ROC(df.ROC,cmrf_par$table,"rf_par")


ggplot(df.ROC, aes(x=X, y=Y, label=modelo)) +
  geom_point() +
  labs(x="X", y="Y", title="Gráfico ROC") + geom_text(hjust=0, vjust=0)
########################## Analise ROC

