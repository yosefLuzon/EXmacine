spam<-read.csv('spamhw.CSV', stringsAsFactors = FALSE)
View(spam)
str(spam)
spam$category <- as.factor(spam$category)

install.packages('tm')
library(tm)
install.packages('wordcloud')
library(wordcloud)
install.packages('e1071')
library(wordcloud)

install.packages('NLP')
library('NLP')
install.packages('RColorBrewer')
library(RColorBrewer)
spam_corpus <- Corpus(VectorSource(spam$text))
print(spam_corpus)
inspect(spam_corpus[1:2])
spam_corpus[[1]][[1]]
spam_corpus[[2]][[2]]
#הסרת סימני פיסוק
clean_corpus <- tm_map(spam_corpus, removePunctuation)
#הסרת רווחים כפולים
clean_corpus <- tm_map(clean_corpus, stripWhitespace)
clean_corpus <- tm_map(clean_corpus, removeWords, stopwords())
clean_corpus <- tm_map(clean_corpus, stripWhitespace)
dtm <- DocumentTermMatrix(clean_corpus)
dim(dtm)
clean_corpus[[1]][[1]]
