
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
weights <- relief(label~., data, neighbours.count = 5, sample.size = 1000)

plot(weights)
print(weights)
Names = cutoff.k(weights,5)
idx = match(Names, names(data))
idx = append(idx,21)
data.relief = data[,idx] 


# SVM
model.svm.completo <- main(data, k=10)
model.svm.pca <- main(data.pca, k=10)
model.svm.relief <- main(data.relief, k=10)

# NN
model.nn.completo <- main.nn(data, k=10)
model.nn.pca <- main.nn(data.pca, k=10)
model.nn.relief <- main.nn(data.relief, k=10)

# NB
model.nb.completo <- main.nb(data, k=10)
model.nb.pca <- main.nb(data.pca, k=10)
model.nb.relief <- main.nb(data.relief, k=10)

data$label[data$label == 'male'] <- 1
data$label[data$label == 'female'] <- 0


data$label <- as.numeric(data$label)

