### Instalação dos pacotes necessários
install.packages('arules',dep=T)
library(arules)
library(datasets)
### Leitura dos dados
dados <-data(Groceries)
inspect(Groceries[1:5])

### Podemos ver a frequência dos 10 primeiros itens:
itemFrequencyPlot(Groceries,topN=10,type='absolute')

### Podemos também ter uma visão geral dos dados:
summary(Groceries)

### Agora vamos obter as regras:
### Primeiramente definimos o Suporte=0,001 e Confiança=0,7
set.seed(1912)
rules<-apriori(Groceries,parameter=list(supp= 0.001,conf= 0.7,minlen=2))
summary(rules)

###Vamos ver as 5 primeiras regras ordenadas pela confiança:
options(digits=2)
inspect(sort(rules[1:5],by="confidence"))

###Se eu desejar saber o que foi comprado com cerveja,
###por exemplo(quem  compra  cerveja  compra  também  quais produtos?)
set.seed(1912)
rules<-apriori(data=Groceries,parameter=list(supp=0.001,conf = 0.1,minlen=2),
              appearance=list(default='rhs',lhs='bottled beer'),control=list(verbose=F))
inspect(sort(rules,by='confidence',decreasing=T))

#########################################################################

##Maquina MP
setwd('C:\\Users\\escneto\\Documents\\Estudos\\Pos_IA_UFPR\\pos_ia_aprendizado_maquina\\Bases_de_teste\\REGRAS_DE_ASSOCIACAO')
##Note
setwd('/Users/MPPR/Documents/Pos_IA/pos_ia_aprendizado_maquina/Bases_de_teste/REGRAS_DE_ASSOCIACAO')

dados <- read.transactions(file="lista_compras.csv",format="basket",sep=";")
inspect(dados[1:4])

### Confiança=0.7, Suporte=0.01
set.seed(728078902)
rules<-apriori(dados,parameter = list(supp=0.01,conf=0.7,target="rules"))
summary(rules)
inspect(rules)
### Confiança=0.1, Suporte=0.001
set.seed(728078902)
rules2<-apriori(dados,parameter = list(supp=0.001,conf=0.1,target="rules"))
summary(rules2)
inspect(rules2)
### Confiança=0.9, Suporte=0.5
set.seed(728078902)
rules3<-apriori(dados,parameter = list(supp=0.5,conf=0.9,target="rules"))
summary(rules3)
inspect(rules3)

##########################################################################
dados <- read.transactions(file="musculacao.csv",format="basket",sep=";")
inspect(dados[1:4])

### Confiança=0.7, Suporte=0.01
set.seed(728078902)
rules<-apriori(dados,parameter = list(supp=0.01,conf=0.7,target="rules"))
summary(rules)
inspect(rules)
### Confiança=0.1, Suporte=0.001
set.seed(728078902)
rules2<-apriori(dados,parameter = list(supp=0.001,conf=0.1,target="rules"))
summary(rules2)
inspect(rules2)
### Confiança=0.9, Suporte=0.5
set.seed(728078902)
rules3<-apriori(dados,parameter = list(supp=0.05,conf=0.9,target="rules"))
summary(rules3)
inspect(rules3)
