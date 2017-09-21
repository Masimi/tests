library('FSelector')
library('mlbench')
library('caret')
data <- read.csv("data/voice.csv", sep = ',',header = TRUE)

#
weights <- relief(label~., data, neighbours.count = 5, sample.size = 3168)
plot(weights)

print(weights)

subset <- cutoff.k(weights,5)


f <- as.simple.formula(subset,"label")

print(f)

######
control <- trainControl(method="repeatedcv", number=10, repeats=3)

model <- train(label~., data, method='rf',trControl=control)

importance <- varImp(model, scale=T)

plot(importance)

