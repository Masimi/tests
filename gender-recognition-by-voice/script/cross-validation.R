#https://gist.github.com/mcmtroffaes/709908

# aquisicao de dados
data <- read.csv("data/voice.csv", sep = ',',header = TRUE)

#converte as classes de string para binario
data$label <- sub("female",1, data$label) #replace string to 1
data$label <- sub("male",0, data$label) #replace string to 0
data$label = as.numeric(data$label) #convert to numeric

# separando eixo x e y
x <- data[,1:20]
y <- data[,21]

#chamada das funcoes
source("script/cv.R")

# separando por k-folds
sizes <- cv.sizes(3168,k=5)

sizes

# obtendos os indices para cada k-fold e balenceado por labels
indices <- cv.partition(y, x, k=5)

df <- data[indices[[5]],]
table(df$label)
