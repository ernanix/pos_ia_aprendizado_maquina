library("caret")

dados <- read.csv(file = '/Users/MPPR/Documents/Pos_IA/pos_ia_aprendizado_maquina/base2.csv')
dados_pred <- read.csv(file = '/Users/MPPR/Documents/Pos_IA/pos_ia_aprendizado_maquina/novos.csv')

dados$id <- NULL
dados_pred$id <- NULL

tuneGrid <- expand.grid(k = c(1,3,5,7,9))

knn <- train(Classe~., data = dados, method = "knn",tuneGrid=tuneGrid)
knn

Val_pred <- predict(knn, dados_pred)

dados_pred$Classe <- NULL
dados_pred <- cbind(dados_pred, Val_pred)
