library("caret")
library(Metrics)


dados <- read.csv(file = '/Users/MPPR/Documents/Pos_IA/pos_ia_aprendizado_maquina/admissao/admissao.csv')

### Cria arquivos de treino e teste
set.seed(728078902)
ind <- createDataPartition(dados$ChanceOfAdmit, p=0.80, list = FALSE)
treino <- dados[ind,]
teste <- dados[-ind,]

### Prepara um grid com os valores de k que serão usados 
tuneGrid <- expand.grid(k = c(1,3,5,7,9))

### Executa o Knn com esse grid
knn <- train(ChanceOfAdmit ~ ., data = treino, method = "knn",
             tuneGrid=tuneGrid)
knn

### Aplica o modelo no arquivo de teste
predict.knn <- predict(knn, teste)

### Mostra as métricas
rmse(teste$ChanceOfAdmit, predict.knn)


r2 <- function(predito, observado) {
  return(1 - (sum((predito-observado)^2) / sum((predito-mean(observado))^2)))
}

r2(predict.knn,teste$ChanceOfAdmit)

### PREDIÇÕES DE NOVOS CASOS
#predict.knn <- predict(knn, dados_novos)
#dados_novos$biomassa <- NULL
#dados_novos <- cbind(dados_novos, predict.knn)


