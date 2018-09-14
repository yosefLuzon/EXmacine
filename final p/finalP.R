


library(ggplot2) # Data visualization
library(readr) 



# import the train dataset
train = read.csv("train.csv")
head(train)
names(train)
dim(train)


# clean the text
library(tm)
library(SnowballC)
corpus = VCorpus(VectorSource(train$comment_text))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords())
#corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)

# Creating the Bag of Words model
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.994)
dataset = as.data.frame(as.matrix(dtm))
head(dataset)
names(dataset)
dim(dataset)
dataset$toxic = NULL
dataset$severe_toxic = NULL
dataset$obscene = NULL
dataset$threat = NULL
dataset$insult = NULL
dataset$identity_hate = NULL
dim(dataset)

# attachh the output variables
dataset$toxic = train$toxic
dataset$severe_toxic = train$severe_toxic
dataset$obscene = train$obscene
dataset$threat = train$threat
dataset$insult = train$insult
dataset$identity_hate = train$identity_hate
dim(dataset)

# create a multiclass variable
attach(dataset)

dataset$target = ifelse(toxic==1,0,NA)
dataset$target = ifelse(severe_toxic==1,1,dataset$target)
dataset$target = ifelse(obscene==1,2,dataset$target)
dataset$target = ifelse(threat==1,3,dataset$target)
dataset$target = ifelse(insult==1,4,dataset$target)
dataset$target = ifelse(identity_hate==1,5,dataset$target)
dataset$target = ifelse(is.na(dataset$target),6,dataset$target)

table(dataset$target)

#remove na rows

dataset = dataset[-which(dataset$target==6),]
table(dataset$target)
dim(dataset)

# import the test datasets
test = read.csv("test.csv")
test_labels = read.csv("test_labels.csv")

# clean the text

corpus = VCorpus(VectorSource(test$comment_text))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords())
#corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)

# Creating the Bag of Words model
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.994)
test_dataset = as.data.frame(as.matrix(dtm))
dim(test_dataset)


# create a multiclass variable
attach(test_labels)

test_dataset$target = ifelse(toxic==1,0,NA)
test_dataset$target = ifelse(severe_toxic==1,1,test_dataset$target)
test_dataset$target = ifelse(obscene==1,2,test_dataset$target)
test_dataset$target = ifelse(threat==1,3,test_dataset$target)
test_dataset$target = ifelse(insult==1,4,test_dataset$target)
test_dataset$target = ifelse(identity_hate==1,5,test_dataset$target)
test_dataset$target = ifelse(is.na(test_dataset$target),6,test_dataset$target)
table(test_dataset$target)


#remove na rows

test_dataset = test_dataset[-which(test_dataset$target==6),]
table(test_dataset$target)
dim(test_dataset)

# match the variables of the traning_set with those of the test_dataset
dim(test_dataset)
dim(dataset)
common_cols = intersect(colnames(dataset),colnames(test_dataset))

# create a dataset with 666 common variables with the test_dataset and train dataset.


dataset1= dataset[common_cols]
dim(dataset1)

test_dataset1 = test_dataset[common_cols]
dim(test_dataset1)


# Encoding the target feature as factor
dataset1$target = dataset$target
dataset1$target = factor(dataset1$target)
# Splitting the dataset into the Training set and Test set
#library(caTools)
library(caret)
set.seed(123)
split = createDataPartition(dataset1$target, p=0.7, list = FALSE)
train_set = dataset1[split,]
test_set = dataset1[-split,]
dim(train_set)
dim(test_set)

# Applying PCA
library(e1071)
pca = preProcess(x = train_set, method = 'pca', pcaComp = 5)
train_set = predict(pca, train_set)
test_set = predict(pca, test_set)
test_dataset2 = predict(pca, test_dataset1)

dim(train_set)split = createDataPartition(dataset2$target, p=0.7, list = FALSE)

dim(test_set)
dim(test_dataset2)
# Fitting Neural netweork to the Training set

set.seed(300)
ctrl = trainControl(method="cv",number=3)
nn_classifier = train(target~.,data=train_set,method="nnet",trControl=ctrl,na.action=na.omit)
y_pred <- predict(nn_classifier, newdata=test_set[-1])
#head(y_pred)
# Making the Confusion Matrix
cm = table(test_set[, 1], y_pred)
confusionMatrix(cm)

# Fitting a naive baeys model to the Training set
nb_classifier = train(target~.,data=train_set,method="nb",trControl=ctrl,na.action=na.omit)


# Fitting a  random forest to the Training set
rf_classifier = train(target~.,data=train_set,method="rf",trControl=ctrl,na.action=na.omit)


# Predicting the Test set results
#which(colnames(test_set)=="target")
y_pred <- predict(rf_classifier, newdata=test_set[-1])
#head(y_pred)
# Making the Confusion Matrix
cm = table(test_set[, 1], y_pred)
confusionMatrix(cm)



# predict on the test dataset
y_pred = predict(nb_classifier, newdata=test_dataset2,type="prob")
y_pred = predict(xgb_classifier, newdata=data.matrix(test_dataset2),reshape=TRUE)
y_pred = as.matrix(y_pred)
head(y_pred)

my_submission<-data.frame(id=test$id,toxic=y_pred[,1],severe_toxic=y_pred[,2],obscene=y_pred[,3],threat=y_pred[,4],insult=y_pred[,5],identity_hate=y_pred[,6])
head(my_submission)
summary(my_submission)
write.csv(my_submission,"svm.csv",row.names = FALSE)
```

