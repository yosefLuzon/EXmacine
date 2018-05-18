#Naive Bayes Algorithm
#Spam filter example

spam<-read.csv('spam.CSV', stringsAsFactors = FALSE)
#will open the dataSet in a table format 
View(spam)
str(spam)
spam$type <- as.factor(spam$type)

install.packages('NLP')
library('NLP')
install.packages('tm')
library(tm)
install.packages('RColorBrewer')
library(RColorBrewer)
install.packages('wordcloud')
library(wordcloud)
install.packages('e1071')
library(e1071)

#Generate DTM
#cleaning the text
spam_corpus <- Corpus(VectorSource(spam$text))
spam_corpus[[1]][[1]]
spam_corpus[[1]][[2]]
spam_corpus[[2]][[1]]
spam_corpus[[2]][[2]]

#הסרת סימני פיסוק
clean_corpus <- tm_map(spam_corpus, removePunctuation)
#הסרת רווחים כפולים
clean_corpus <- tm_map(clean_corpus, stripWhitespace)
#אותיות גדולות לקטנות
clean_corpus <- tm_map(clean_corpus, content_transformer(tolower))
#הסרת מילים תדירות בשימוש
clean_corpus <- tm_map(clean_corpus, removeWords, stopwords())
clean_corpus <- tm_map(clean_corpus, stripWhitespace)

dtm <- DocumentTermMatrix(clean_corpus)

dim(dtm)

clean_corpus[[1]][[1]]

#remove infrequent words
frequent_dtm <- DocumentTermMatrix(clean_corpus, list(dictionary = findFreqTerms(dtm,10)))
dim(frequent_dtm)

#Data visualization
pal <- brewer.pal(9, 'Dark2')

wordcloud(clean_corpus, min.freq = 5, random.order = FALSE, colors = pal)
#spam
wordcloud(clean_corpus[spam$type == 'spam'], min.freq = 5, random.order = FALSE, colors = pal)
#ham
wordcloud(clean_corpus[spam$type == 'ham'], min.freq = 5, random.order = FALSE, colors = pal)

#Spliting into training and test set
split <- runif(500)
split <- split > 0.3

#dividing raw data
train_raw <- spam[split,]
test_raw <- spam[!split,]

#dviding clean corpus
train_corpus <- clean_corpus[split]
test_corpus <- clean_corpus[!split]

#dividing dtm
train_dtm <- frequent_dtm[split,]
test_dtm <- frequent_dtm[!split,]

#convert the DTM into yes/no
conv_yesno <- function(x){
  x <- ifelse(x>0,1,0)
  x <- factor(x,levels = c(1,0), labels = c('Yes', 'No'))
}

train <- apply(train_dtm, MARGIN = 1:2,conv_yesno)
test <- apply(test_dtm, MARGIN = 1:2,conv_yesno)

#converting into a data frame
df_train <- as.data.frame(train)
df_test <- as.data.frame(test)

#add the type column
df_train$type <- train_raw$type
df_test$type <- test_raw$type

dim(df_train)

df_train[,58]

#Generating the model using naive bayes
model <- naiveBayes(df_train[,-58], df_train$type)

prediction <- predict(model,df_test[,-58])

install.packages('SDMTools')
library('SDMTools')
install.packages('R.00')
library('R.00')

conv_10<-function(x){
  return (ifelse(x=='spam',1,0))
}
pred10<-sapply(prediction, conv_10)
actual10<-sapply(df_test$type, conv_10)

