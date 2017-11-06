#https://gist.github.com/mcmtroffaes/709908

# aquisicao de dados
data <- read.csv("data/voice.csv", sep = ',',header = TRUE)

#converte as classes de string para binario
data$label <- sub("female",1, data$label) #replace string to 1
data$label <- sub("male",0, data$label) #replace string to 0
data$label = as.numeric(data$label) #convert to numeric

# separando eixo x e y
x <- data[,1:20]
label <- data$label
label <- as.data.frame(label)

#chamada das funcoes
source("script/cv.R")

# separando por k-folds
sizes <- cv.sizes(3168,k=5)

sizes

# obtendos os indices para cada k-fold e balenceado por labels
indices <- cv.partition(y, x, k=5)

df <- data[indices[[5]],]
table(df$label)

source("script/crossValidation.R")

pca <- princomp(data[,1:20],cor = T,scores = T)

plot(pca)

data.pca <- cbind(pca$scores[,1:5],label)

svm_pca <- main(data.pca,k=10)


df <- cross.partition(data = data, k = 100)

