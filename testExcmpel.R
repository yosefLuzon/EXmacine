
#GGPLOT
install.packages('ggplot2')
library(ggplot2)

install.packages("ggplot2movies")
library(ggplot2movies)

#
install.packages('Amelia')
library(Amelia)

#
install.packages('dplyr')
library(dplyr)

#for correlations 
install.packages("corrplot")
library(corrplot)
##part of corrplot package, just need the library
library(caTools)

install.packages('ISLR')
library('ISLR')

install.packages('cluster')
library(cluster)

install.packages('RSQLite')
install.packages('sqldf')
library(RSQLite)
library(sqldf)
install.packages('tm')
library(tm)


###1
dataSet1 <- read.csv("testSet.CSV" ,stringsAsFactors = FALSE)
str(dataSet1)
head(dataSet1)
dataSet1$category <- as.factor(dataSet1$category)
dataSet1_corpus <- Corpus(VectorSource(dataSet1$text))
### remove puctuation

clean_corpus <- tm_map(dataSet1_corpus,removePunctuation)

#remove digits
clean_corpus <- tm_map(dataSet1_corpus,removeNumbers)
#turn to lower case
clean_corpus <- tm_map(dataSet1_corpus,content_transformer(tolower))

#remove stopwords 
clean_corpus <- tm_map(dataSet1_corpus,removeWords, stopwords())

#  Multiple whitespace characters are collapsed to a single blank
clean_corpus <- tm_map(dataSet1_corpus,stripWhitespace)
stopwords()
dtm <- DocumentTermMatrix(clean_corpus)
#inspet the dtm 
dim(dtm)
#removing infrequent terms (that do not apear at least 10 times)
frequent_dtm <-DocumentTermMatrix(clean_corpus,list(dictionary=findFreqTerms(dtm,6)))

#inspet the frequent_dtm
dim(frequent_dtm)
print(frequent_dtm)
######2

#convert the frequency matrix to yes/no  
conv_yesno <- function(x){
  x <- ifelse(x>0,1,0)
  x <- factor(x, level = c(1,0), labels = c('Yes', 'No'))
}

test <- apply(frequent_dtm, MARGIN = 1:2, conv_yesno)
df_test = as.data.frame(test)
#add target columns

#append the category colums 
df <- cbind(df_test, dataSet1$category)
#Check structure 
str(df)
model.naive <- naiveBayes(df[,-18],df$`dataSet1$category`)

###3
predicted <- predict(model.naive,df)


#look for errors 
table(predicted,df$`dataSet1$category`)

frequent_dtm
###4

frequent_dtm
dtm<- as.matrix(frequent_dtm)
dtm1<-as.data.frame(dtm)
dtm2<-cbind(dtm1,dataSet1$category)
dtm2$category<-dtm2$`dataSet1$category`
dtm2$`dataSet1$category`<-NULL
str(dtm2)
dtm2$category<-as.numeric(dtm2$category)
dtm2$category<-as.factor(dtm2$category)
model.log <- glm(category ~ ., family = binomial(link = 'logit'), data = dtm2)
##5
str(dtm2$category)
kdtm2<-kmeans(dtm2[ ,1:7],2, nstart = 10)

install.packages('cluster')
library(cluster)
table(kdtm2$cluster, dtm2$category)

clusplot(kdtm2,kdtm2$cluster, color=T, shade=T, labels=0, lines=0)
