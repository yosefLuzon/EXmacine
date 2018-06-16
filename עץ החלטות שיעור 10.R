install.packages('rpart')
library(rpart)
install.packages('ropart.plot')
library(rpart.plot)
str(kyphosis)
#רוצים לבדוק מה האם אפשר לחזות מתי הניתוח יצליח
tree<-rpart(Kyphosis ~ .,kyphosis)
prp(tree)
predicted<-predict(tree,kyphosis)
