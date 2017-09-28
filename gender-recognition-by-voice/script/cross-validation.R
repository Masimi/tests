#https://gist.github.com/mcmtroffaes/709908
library(caret)

data <- read.csv("data/voice.csv", sep = ',',header = TRUE)

x <- data[,1:20]
y <- data[,21]

#converte as classes de string para binario
data$label <- sub("female",1, data$label) #replace string to 1
data$label <- sub("male",0, data$label) #replace string to 0
data$label = as.numeric(data$label) #convert to numeric

#separando amostras de treino e test
set.seed(1234)
trainIndex <- createDataPartition(data$label, p = 0.7, list = FALSE, times = 1)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

#folds
control <- trainControl(method="cv", number=10)

#train
model <- train(label~.,data = train, trControl = control, method = "rpart")

predict <- predict(model,test)

print(model)

source("script/cv.R")

sizes <- cv.sizes(3168,k=5)

values <- 1:3168

sample(values,sizes[1])

createFolds(data,k=5)

cv.testing(3168,k=5)


cv.partion(y, x, p=0.7, k=5)


indices = list()
n = length(y)
label <- c(unique(y))

sizes = cv.sizes(n,k=5)
values = 1:n

for (i in 1:length(label)){
  indices = append(indices, label[i])
}


indices[2]

