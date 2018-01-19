library('caret')

##########################
# Data Aquisition
##########################
df <- read.csv("data/creditcard.csv",sep = ",")

##########################
# Inspect data
##########################
head(df)
sapply(df,class)
summary(df)

df$Time <- NULL
##########################
# Dataset Dimmensions
##########################
dim(df)
table(df$Class)

##########################
# Checking missing values
##########################
summary(is.na(df))

##########################
# Normalize 
##########################
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df$Amount <- normalize(df$Amount)

#################################################
# Inspecting the correlation among each features
#################################################

cor(df)

######################################
# Split dataset in training and test
######################################
set.seed(1234)

trainIndex <- createDataPartition(df$Class, p = 0.7, list = FALSE, times = 1)
train <- df[trainIndex, ]
test <- df[-trainIndex, ]

View(df) 

###########################
# Descriptive analysis
###########################

prop.table(table(train$Class))
sapply(train,summary)

par(mfrow = c(1, 2))
with(train[train$Class == "1", ], hist(V1, main = "Fraud", col = "cyan"))
with(train[train$Class == "0", ], hist(V1, main = "Otherwise", col = "cyan"))


##########################
# Linear regression model
##########################
fit <- lm(Class~., data = train)

linearModel <- predict(fit, type='response')
t1 <- table(train$Class, linearModel >= 0.5)
accuracy_train <- (t1[1, 1] + t1[2, 2])/sum(t1)
accuracy_train

linearModel2 <- predict(fit, newdata = test, type='response')
t2 <- table(test$Class, linearModel2 >= 0.5)
accuracy_test <- (t2[1, 1] + t2[2, 2])/sum(t2)
accuracy_test

##########################
# Resampling
##########################
#Number of data points in the minority class
count_frauds <- length(which(df$Class == 1))
fraud_indices <- which(df$Class == 1)

#Picking the indices of the normal classes
normal_indices <- which(df$Class == 0)

#Out of the indices we picked, randomly select "x" number (number_records_fraud)
randon_normal_indices <- sample(normal_indices,count_frauds, replace = FALSE)

#Appending the 2 indices
samples_indices <- c(fraud_indices,randon_normal_indices)

#Under sample dataset
sample_df <- df[samples_indices,]

#Convert class
sample_df$Class <- as.numeric(sample_df$Class)
##########################
# Dimmensions
##########################

sapply(sample_df,class)

table(sample_df$Class)

######################################
# Split sampled dataset in training and test
######################################
set.seed(1234)
sample_trainIndex <- createDataPartition(sample_df$Class, p = 0.7, list = FALSE, times = 1)
sample_train <- sample_df[sample_trainIndex, ]
sample_test <- sample_df[-sample_trainIndex, ]

##########################
# Linear regression model
##########################
model.lm <-  lm(Class~., data=sample_train)
prediction.lm <- predict(model.lm, sample_test, type='response')

# Confussion Matrix
conf_matrix.lm <- table(sample_test$Class, prediction.lm >= 0.5)
conf_matrix.lm

#Accuracy
accuracy.lm <- (conf_matrix.lm[1,1] + conf_matrix.lm[2,2])/sum(conf_matrix.lm)
accuracy.lm


############################
# Logistic regression model
############################
model.glm <- glm(Class~., data=sample_train, family = binomial())
prediction.glm <- predict(model.glm, sample_test)

# Confussion Matrix
conf_matrix.glm <- table(sample_test$Class, prediction.glm >= 0.5)
conf_matrix.glm

#Accuracy
accuracy.glm <- (conf_matrix.glm[1, 1] + conf_matrix.glm[2, 2])/sum(conf_matrix.glm)
accuracy.glm

#Fim