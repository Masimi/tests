
library('FSelector')

# carregando as funcoes
source("script/crossValidation.R")

# aquisicao de dados
data <- read.csv("data/voice.csv", sep = ',',header = TRUE)

# Selecao de caracteristicas utilizando PCA

label <- factor(data$label)

pca <- princomp(data[,1:20], cor=TRUE, scores = TRUE)

plot(pca)

plot(pca$scores[,1:5], col = label)

data.pca <- cbind.data.frame(pca$scores[,1:5], label)

colnames(data.pca)

# Selecao de caracteristicas utilizando Relief
weights <- relief(label~., data, neighbours.count = 5, sample.size = 3168)
plot(weights)

print(weights)

data.relief <- cutoff.k(weights,5)

# svm
#particoes <- cross.partition(data , k=5)

model.svm <- main(data.pca, k=10)



xpto <- cross.partition(data, 5)

for (i in 1:length(xpto)) {
  
  print(table(part[[1]]$label))
}

xpto[[1]]

sample(10, 3)



subset(data, )

x[y==predictors[c,1],]

sample( y[y=="female",],5 )




