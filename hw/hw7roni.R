install.packages('caTools')
install.packages('ISLR')
library(ISLR)
install.packages('ggplot2')
library(caTools)
library(ggplot2)

ggplot(df, aes(Room.Board,Grad.Rate)) + geom_point(aes(color = Private), size = 4, alpha = 0.5)


df <- College
str(df)
?College
ggplot(df, aes(Grad.Rate)) + geom_histogram(aes(fill =Private),color= 'red', binwidth = 1)
ggplot(df, aes(F.Undergrad)) + geom_histogram(aes(fill =Private),color= 'red',  bins = 50)

filter <- df[,"Grad.Rate"] >= 100
df[filter,]


df['Cazenovia College', 'Grad.Rate'] <- 100

sample <- sample.split(df,0.7)

train <- df[sample == T,]
test <- df[sample == F,]
tree <- rpart(Private ~ .,method = 'class' ,data = train)
??prp

tree.preds <- predict(tree,test)

head(tree.preds)

tree.preds <- as.data.frame(tree.preds)

joiner <- function(x){
  if(x > 0.5){
    return('PYes')
  }else{
    return('PNo')
  }
}

tree.preds$private <- sapply(tree.preds$Yes, joiner)

head(tree.preds)

confusion <- table(tree.preds$private, test$Private)

class(confusion)

TN <- confusion[1,1]
FN <- confusion[1,2]
FP <- confusion[2,1]
TP <- confusion[2,2]

recall <- TP/(TP+FN)
precision <- TP/(TP+FP)


