library(deal)
library('caret')
library(e1071)

# Aquire data
data <- read.csv("data/voice.csv", sep = ',',header = TRUE)

# split data set in train and test
trainIndex <- createDataPartition(data$label, p = 0.7, list = FALSE, times = 1)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

#

model = naiveBayes(label~.,train)

class(model)
summary(model)

preds = predict(model, newdata = test)

conf_matrix <- table(preds, test$label)

conf_matrix

table(predict(model,test))


prior = jointprior(nw,3168)
fit.learn <- learn(nw, data, prior)
fit.nw = getnetwork(fit.learn)

fit.learn$trylist

fit.nw$score

test = drawnetwork(nw,data,prior)


rats.nw <- network(rats)
rats.prior <- jointprior(rats.nw,12)
rats.nw <- getnetwork(learn(rats.nw,rats,rats.prior))
newrat <- getnetwork(drawnetwork(rats.nw,rats,rats.prior))

score(rats.nw)


