library("caret")

dados <- read.csv(file = '/Users/MPPR/Documents/Pos_IA/pos_ia_aprendizado_maquina/alunos/alunos.csv')
dados_novos <- read.csv(file = '/Users/MPPR/Documents/Pos_IA/pos_ia_aprendizado_maquina/alunos/alunos_novos.csv')

### Cria arquivos de treino e teste
set.seed(728078902)
ind <- createDataPartition(dados$G3, p=0.80, list = FALSE)
treino <- dados[ind,]
teste <- dados[-ind,]

### Prepara um grid com os valores de k que serão usados 
tuneGrid <- expand.grid(k = c(1,3,5,7,9))

### Executa o Knn com esse grid
knn <- train(G3 ~ ., data = treino, method = "knn",
             tuneGrid=tuneGrid)
knn

### Aplica o modelo no arquivo de teste
predict.knn <- predict(knn, teste)

### Mostra as métricas
rmse(teste$G3, predict.knn)


r2 <- function(predito, observado) {
  return(1 - (sum((predito-observado)^2) / sum((predito-mean(observado))^2)))
}

r2(predict.knn,teste$G3)

### PREDIÇÕES DE NOVOS CASOS
predict.knn <- predict(knn, dados_novos)
dados_novos$G3 <- NULL
dados_novos <- cbind(dados_novos, predict.knn)


