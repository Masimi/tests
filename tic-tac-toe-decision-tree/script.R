library('caret')
library('rpart.plot')

tic_tac_toe = read.delim('data/tic-tac-toe.data.txt', header = F, sep = ',')
names(tic_tac_toe) = c(
  'top_left',
  'top_middle',
  'top_right',
  'middle_left',
  'middle_middle',
  'middle_right',
  'bottom_left',
  'bottom_middle',
  'bottom_right',
  'label'
)

anyNA(tic_tac_toe)
dim(tic_tac_toe)

sapply(tic_tac_toe,class)

tic_tac_toe[,1:9] <- sapply(tic_tac_toe[,1:9],as.character)

table(tic_tac_toe$label)

summary(tic_tac_toe)

#Split data in test and traning sets

trainingIndex = createDataPartition(tic_tac_toe$label,p=0.7,list = FALSE, times = 1)
train_ttt = tic_tac_toe[trainingIndex,]
test_ttt = tic_tac_toe[-trainingIndex,]

table(train_ttt)

#Training decision tree
set.seed(1234)
ctrl = trainControl(method = "cv", number=10)

model.tree = train(label~.,data= train_ttt,
                   method = 'rpart',
                   trControl = ctrl)


model.tree

prp(model.tree$finalModel, box.palet = "Reds")


#Print model tree
rpart.plot(model.tree$finalModel)

#Prediction

test_predict = predict(model.tree,test_ttt)

confusionMatrix(test_predict,test_ttt$label)
