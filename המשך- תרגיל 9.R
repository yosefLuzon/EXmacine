dataset<-read.csv("dataset.CSV", stringsAsFactors = FALSE)
str(dataset)
####1
dataset$category<-as.factor(dataset$category)
text_corpus<-Corpus(VectorSource(dataset$text))

clean_corpus<-tm_map(text_corpus, removePunctuation)
clean_corpus[[1]][[1]]
clean_corpus<-tm_map(clean_corpus, stripWhitespace)
clean_corpus<-tm_map(clean_corpus, content_transformer(tolower))
clean_corpus<-tm_map(clean_corpus, removeWords,stopwords())
dtm <- DocumentTermMatrix(clean_corpus)
dim(dtm)
frequent_dtm<-DocumentTermMatrix(clean_corpus, list(dictionary=findFreqTerms(dtm,4)))
dim(frequent_dtm)
dtm <- as.matrix(frequent_dtm)

dtm.df <- as.data.frame(dtm)

df.tree <- cbind(dtm.df,dataset$category)
str(df.tree)
#change name of the category colums 
df.tree$category <- df.tree$`dataset$category`
df.tree$`dataset$category` <- NULL 
str(df.tree)
####
install.packages('caret ')
library(caret )
install.packages('rpart')
library(rpart)
install.packages('randomForest')
library(randomForest)
install.packages('pROC')
library(pROC)
#####
tree<-rpart(category~ . , df.tree)
prp(tree)

predictedTree<-predict(tree,df.tree)
print(tree)
treerandom<-randomForest(category~ . ,data=df.tree)
print(treerandom)
predictedRandomTree<-predict(treerandom,df.tree)
str(df.tree)
predictedTree<-predict(tree,df.tree,type='prob')
class(df.tree)
df.tree<-as.data.frame(df.tree)
roca<-roc(predictedTree[ ,'sport'],df.tree$category,levels =c('economy','Sport')

plot(roca,col="red", main ="q4")
