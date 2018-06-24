
#install library and package 
install.packages('tm')
library(tm)

install.packages('e1071')
library(e1071)

#set working directory
setwd("C:/Users/user/Dropbox/Public/jce/כריית ידע/תשעח/R examples")

contact<-read.csv('spamhw.CSV', stringsAsFactors = FALSE)
#read the file 
contact<-read.csv('contact.csv', stringsAsFactors = FALSE)
str(contact)

#Make category a factor 
contact$category <- as.factor(contact$category )

#look at the existing category levels 
levels(contact$category)

#It turns out there are records with empty category 
#Let's remove them 

filter_not_empty <- contact$category != ""

#Let's create a new df without the empty rows  
contact_new <- contact[filter_not_empty, ]

#Look at levels again 
levels(contact_new$category)

#Aalthogh we removed the rows with empty category
# The empty level is still there. let's remove it

#droplevels removes unused levels
contact_new <- droplevels(contact_new)


#Generate five addtional colums to add the the df 
# for the diffrent on-all tests 

#A function to add a column 
add_column <- function(df,category){
  new_column <- ifelse(df$category == category,1,0)
  new_column <- as.factor(new_column)
  df[category] <- new_column
  return(df)
}

#Actually adding the columns 
contact_new <- add_column(contact_new,'sales')
contact_new <- add_column(contact_new,'block')
contact_new <- add_column(contact_new,'coop')
contact_new <- add_column(contact_new,'stop')
contact_new <- add_column(contact_new,'support')

#testing that we actually added the columns 
str(contact_new)

#isolate text column 
text <- contact_new$text

#build the corpus pay attention to Hebrew
contact.corpus <- Corpus(VectorSource(text), readerControl = list(language = "heb"))

#build DTM 
dtm <- DocumentTermMatrix(contact.corpus)

#inspect dtm 
inspect(dtm)

#Build the stopword hebrew file 
#first look at the most frequent terms in the document
m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing=TRUE)

#generate stopwords from words that are 1)frequent and 
#2)does not seem to carry any  meaning 

stopwords <- c(v[1],v[7],v[30])


# cleaning the corpus
# remove punctuation
clean_corpus <- tm_map(contact.corpus,removePunctuation)
dtm_test <- DocumentTermMatrix(clean_corpus)
inspect(dtm_test)

#  Multiple whitespace characters are collapsed to a single blank
clean_corpus <- tm_map(clean_corpus,stripWhitespace)
dtm_test <- DocumentTermMatrix(clean_corpus)
inspect(dtm_test)

#remove digits
clean_corpus <- tm_map(clean_corpus,removeNumbers)
dtm_test <- DocumentTermMatrix(clean_corpus)
inspect(dtm_test)

#turn to lower case (not very important in hebrew)
clean_corpus <- tm_map(clean_corpus,content_transformer(tolower))
dtm_test <- DocumentTermMatrix(clean_corpus)
inspect(dtm_test)

#remove stopwords 
clean_corpus <- tm_map(clean_corpus,removeWords, stopwords)
dtm_test <- DocumentTermMatrix(clean_corpus)
inspect(dtm_test)


#generate the document matrix 
dtm <- DocumentTermMatrix(clean_corpus)
inspect(dtm)


#inspect the dtm 
dim(dtm)

#removing infrequent terms (that do not appear at least 20 times)
#this is beacuse  be one of the most important tuning parameters later

frequent_dtm <-DocumentTermMatrix(clean_corpus,list(dictionary=findFreqTerms(dtm,20)))


#For some reason this damaged Hebrew..

#Getting the frequent terms in another way
dtm_mat <- as.matrix(dtm)

sums <- colSums(dtm_mat)
column_filter <- sums > 20

dtm_mat_final <- dtm_mat[,column_filter]

#Let's see what came out:
dim(dtm_mat_final)
dtm_mat_final[1:5,1:5]


#Splitting to training and testing data sets
vec <- runif(nrow(dtm_mat_final))
split <- vec > 0.3


#splitting the raw data
train_raw <- contact_new[split,]
dim(train_raw)
test_raw <- contact_new[!split,]
dim(test_raw)


#splitting document term matrix 
train_dtm <- dtm_mat_final[split,]
test_dtm <- dtm_mat_final[!split,]
nrow(train_dtm)
nrow(test_dtm)


#convert the frequency matrix to yes/no  
conv_yesno <- function(x){
  x <- ifelse(x>0,1,0)
  x <- factor(x, level = c(1,0), labels = c('Yes', 'No'))
}

train <- apply(train_dtm, MARGIN = 1:2, conv_yesno)
test <- apply(test_dtm, MARGIN = 1:2, conv_yesno)


dim(train)
dim(test)


#convert the matrix into data frames 
df_train = as.data.frame(train)
df_test = as.data.frame(test)

#add target columns

df_train <- cbind(df_train,train_raw[,2:7])
dim(df_train)

df_test <- cbind(df_test,test_raw[,2:7])
dim(df_test)


# producing a model for each of the one-vs-all cases

model.sales <- naiveBayes(df_train[,1:226],df_train$sales)
model.block <- naiveBayes(df_train[,1:226],df_train$block)
model.coop <- naiveBayes(df_train[,1:226],df_train$coop)
model.stop <- naiveBayes(df_train[,1:226],df_train$stop)
model.support <- naiveBayes(df_train[,1:226],df_train$support)

#test each model separately 
pred.sales <- predict(model.sales, df_test[,1:226])
pred.sales.raw <-predict(model.sales, df_test[,1:226], "raw")
table(pred.sales,df_test$sales)

pred.block <- predict(model.block, df_test[,1:226])
pred.block.raw <- predict(model.block, df_test[,1:226], "raw")
table(pred.block,df_test$block)

pred.coop <- predict(model.coop, df_test[,1:226])
pred.coop.raw <- predict(model.block, df_test[,1:226], "raw")
table(pred.coop,df_test$coop)


pred.stop <- predict(model.stop, df_test[,1:226])
pred.stop.raw <- predict(model.stop, df_test[,1:226], "raw")
table(pred.stop,df_test$stop)


pred.support <- predict(model.support, df_test[,1:226])
pred.support.raw <- predict(model.support, df_test[,1:226], "raw")
table(pred.support,df_test$support)

models <- list(model.sales,model.block, model.coop, model.stop, model.support)
df <- df_test[,1:226];

#The functions that aggregates all the models

predict.all <- function(models,df){
  pred.sales.raw <-predict(models[[1]], df, "raw")[,2] 
  pred.block.raw <-predict(models[[2]], df, "raw")[,2]
  pred.coop.raw <-predict(models[[3]], df, "raw")[,2]
  pred.stop.raw <-predict(models[[4]], df, "raw")[,2]
  pred.support.raw <-predict(models[[5]], df, "raw")[,2]
  mat <- rbind(pred.sales.raw,pred.block.raw,pred.coop.raw,pred.stop.raw,pred.support.raw)
  names <- c('sales','block', 'coop', 'stop', 'support')
  rownames(mat) <- names
  #find the row name with the highest value
  return (rownames(mat)[apply(mat,2,which.max)])
}

predicted <- predict.all(models,df_test[,1:226])


#look for errors 
table(predicted,test_raw$category)

      
      