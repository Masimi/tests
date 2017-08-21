library(corrplot)
library(caret)
library(MASS)

# Data Aquisition
df <- read.csv("data/voice.csv", sep = ',',header = TRUE)

df <- na.omit(df)

# Inspect data
sapply(df,class)

# Dataset Dimmensions
sprintf("Records: %s; Variables: %s", dim(df)[1], dim(df)[2])
table(df$label)

#Cheking missing values
summary(is.na(df))

# Convert class "label" to numeric
df$label <- sub("female",1, df$label) #replace string to 1
df$label <- sub("male",0, df$label) #replace string to 0
df$label = as.numeric(df$label) #convert to numeric

# Check the correlaion betrween each features
cor(df[,1:20])

# Split data in training and test
set.seed(1234)
trainIndex <- createDataPartition(df$label, p = 0.7, list = FALSE, times = 1)
train <- df[trainIndex, ]
test <- df[-trainIndex, ]

# Análise descritiva
prop.table(table(train$label))
sapply(train,summary)

par(mfrow = c(1, 2))
with(train[train$label == "1", ], hist(meanfreq,  main = "female", col = "cyan"))
with(train[train$label == "0", ], hist(meanfreq, main = "male", col = "cyan"))

help(hist)
help(lm)

#Scale
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#traindNorm <- preProcess(train[,1:20], method=c("center", "scale"))
trainNorm <- as.data.frame(lapply(train[,1:20],normalize))
trainNorm$label <- train$label

newdatacor = cor(trainNorm[,1:10])
corrplot(newdatacor, method = "number")



# Fitting model
#fit <- lm(train$label ~ ., data = train[,1:20])
fit <- lm(label~., data = train)
fit2 <- glm(label~., data = train, family=binomial())

Lmod <- predict(fit, type = "response")


print('Logistic regression model')

genderLog <- glm(label ~ ., data=train, family='binomial')

# Accuracy: 0.9711
predictLog <- predict(fit, type='response')
table(train$label, predictLog >= 0.5)
(1073+1081)/nrow(train)

# Accuracy: 0.9789
predictLog2 <- predict(fit, newdata=test, type='response')
table(test$label, predictLog2 >= 0.5)
(462+468)/nrow(test)


#predictGender <- predict(fit, type = "response")
predictGender <- predict.glm(fit2, newdata = test, type="response")

predict.lm(fit, newdata = test)

predictGender

confusionMatrix(predictGender,test$label)$overall[1]

str(predictGender)

complete.cases(test)



