install.packages('rpart')
library(rpart)
install.packages('ropart.plot')
library(rpart.plot)
str(kyphosis)
#����� ����� �� ��� ���� ����� ��� ������ �����
tree<-rpart(Kyphosis ~ .,kyphosis)
prp(tree)
predicted<-predict(tree,kyphosis)
