library(caret)

###############
# Data Aquisition
###############
df <- read.csv("data/voice.csv", sep = ',',header = TRUE)

###############
# Inspect data
###############
sapply(df,class)

###############
# Dataset Dimmensions
###############
sprintf("Records: %s; Variables: %s", dim(df)[1], dim(df)[2])
table(df$label)

###############
#Cheking missing values
###############
summary(is.na(df))

###############
# Convert class "label" to numeric
###############
df$label <- sub("female",1, df$label) #replace string to 1
df$label <- sub("male",0, df$label) #replace string to 0
df$label = as.numeric(df$label) #convert to numeric

###############
# Check the correlaion betrween each features
###############
cor(df[,1:20])

###############
# Split data in training and test
###############
set.seed(1234)
trainIndex <- createDataPartition(df$label, p = 0.7, list = FALSE, times = 1)
train <- df[trainIndex, ]
test <- df[-trainIndex, ]

###############
# Análise descritiva
###############
prop.table(table(train$label))
sapply(train,summary)

par(mfrow = c(1, 2))
with(train[train$label == "1", ], hist(meanfreq,  main = "female", col = "cyan"))
with(train[train$label == "0", ], hist(meanfreq, main = "male", col = "cyan"))

###############
# Linear regression model
###############
fit <- lm(label~., data = train)

linearModel <- predict(fit, type='response')
t1 <- table(train$label, linearModel >= 0.5)
accuracy_train <- (t1[1, 1] + t1[2, 2])/sum(t1)
accuracy_train

linearModel2 <- predict(fit, newdata = test, type='response')
t2 <- table(test$label, linearModel2 >= 0.5)
accuracy_test <- (t2[1, 1] + t2[2, 2])/sum(t2)
accuracy_test

###############
# Logistic regression model
###############
fit2 <- glm(label~., data = train, family=binomial())

logisticModel <- predict(fit2, type='response')
t3 <- table(train$label, logisticModel >= 0.5)
accuracy_log_train <- (t3[1, 1] + t3[2, 2])/sum(t3)
accuracy_log_train

logisticModel2 <- predict(fit2, newdata = test, type='response')
t4 <- table(test$label, logisticModel2 >= 0.5)
accuracy_log_test <- (t4[1, 1] + t4[2, 2])/sum(t4)
accuracy_log_test
