install.packages('randomForest')
library(randomForest)
install.packages('pROC')
library(pROC)
str(kyphosis)
kyphosis

rf.model <- randomForest(Kyphosis ~ ., data = kyphosis)

print(rf.model)
predicted <-predict(rf.model, kyphosis)

predictedProb <- predict(rf.model, kyphosis, type = 'prob')
predictedProb[, 'present'] 

install.packages("pROC")
library(pROC)

rocCurve <-roc(kyphosis$Kyphosis,predictedProb[,'present'],levels=c("absent","present"))

plot(rocCurve, col = "red", main = "Roc Chart")

