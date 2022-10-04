library("caret")
library("mice")

##Maquina MP
setwd('C:\\Users\\escneto\\Documents\\Estudos\\Pos_IA_UFPR\\pos_ia_aprendizado_maquina\\Bases_de_teste')
barra ="\\"
##Note
setwd('/Users/MPPR/Documents/Pos_IA/pos_ia_aprendizado_maquina/Bases_de_teste')
barra ="/"

dados <- read.csv(file = paste('previsao_tempo','previsao_tempo.csv',sep =barra))
dados_novos <- read.csv(file = paste('previsao_tempo','previsao_tempo_novos.csv',sep =barra))

### Set Seed
set.seed(728078902)

### Cria arquivos de treino e teste
ran <- sample(1:nrow(dados), 0.8 * nrow(dados))
treino <- dados[ran,] 
teste <- dados[-ran,] 

########################## KNN
### Faz um grid com valores para K e Executa o KNN
tuneGrid <- expand.grid(k = c(1,3,5,7,9))

knn <- train(Chovera~., data = treino, method = "knn",tuneGrid=tuneGrid)
knn

### Faz a predição e mostra a matriz de confusão
predict.knn <- predict(knn, teste)
confusionMatrix(predict.knn, as.factor(teste$Chovera))

### PREDIÇÕES DE NOVOS CASOS

predict.knn <- predict(knn, dados_novos)
dados_novos$Chovera <- NULL
dados_novos <- cbind(dados_novos, predict.knn)
########################## KNN

########################## RNA
imp <- mice(dados)
dados <- complete(imp,1)

########## Treinar o modelo com Hold-out
rna <- train(Chovera~.,data=treino,method="nnet",trace=FALSE)
rna

### Faz a predição e mostra a matriz de confusão
predict.rna <-predict(rna,teste)
confusionMatrix(predict.rna,as.factor(teste$Chovera))

###Cross Validation
ctrl <- trainControl(method = "cv", number = 10)
rna_cv <- train(Chovera~.,data=treino,method="nnet",trace=FALSE,trControl=ctrl)
rna_cv
predict.rna_cv <- predict(rna_cv,teste)
confusionMatrix(predict.rna_cv,as.factor(teste$Chovera))


###Parametrização
grid <-expand.grid(size=seq(from=1,to=45,by=10),decay=seq(from=0.1, to=0.9, by=0.3))
rna_par <- train(form=Chovera~., data=treino, method="nnet", tuneGrid=grid, trControl=ctrl, maxit=2000, trace=FALSE)
rna_par

predict.rna_par <- predict(rna_par,teste)
confusionMatrix(predict.rna_par,as.factor(teste$Chovera))
########################## RNA