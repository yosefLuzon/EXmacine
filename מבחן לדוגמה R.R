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
install.packages('wordcloud')
library(wordcloud)
pal<-brewer.pal(9,'Dark2')
wordcloud(clean_corpus, min.freq = 2,random.order = FALSE, colors = pal)
####2
#Function to convert
conv_yesno <- function(x){
  x <- ifelse(x>0,1,0)
  x <- factor(x, level = c(1,0), labels = c('Yes', 'No'))
}


#Apply the function 
dtm.yn <- apply(frequent_dtm, MARGIN = 1:2, conv_yesno)

#convert to data frame 
dtm.yn.df <- as.data.frame(dtm.yn)
df <- cbind(dtm.yn.df, dataset$category)
str(df)
####
install.packages('e1071')
library(e1071)
install.packages('SPMTool')
library(e1071)
str(df)
model<-naiveBayes(df[ ,-18],df$`dataset$category`)

####3
predict<-predict(model,df[ ,-18])
table(predict,df$`dataset$category`)

####4
dtm <- as.matrix(frequent_dtm)

dtm.df <- as.data.frame(dtm)

df.log <- cbind(dtm.df,dataset$category)
str(df.log)
#change name of the category colums 
df.log$category <- df.log$`dataset$category`
df.log$`dataset$category` <- NULL 
str(df.log)
log.model<-glm(category~. ,family=binomial(link='logit'),data=df.log)
summary(log.model)

##5
predict.prod<-predict(log.model,df.log,type='response')
cm=table(df.log$category,predict.prod>0.5)

##6
kmean.k<-kmeans(df.log[ ,1:17],2,nstart=20)
table(kmean.k$cluster,df.log$category)
clusplot(df.log,kmean.k$cluster,color=T,shade=T,labely=0, lines = 0)

