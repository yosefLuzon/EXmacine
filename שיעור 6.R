#Logistic regression titanic example 
df.titanic <-  read.csv("titanic.csv")

str(df.titanic)
head(df.titanic)
summary(df.titanic)


#Getting an overview re missing values
install.packages('Amelia')
library(Amelia)
missmap(df.titanic, main = "Missing data", col = c('yellow', 'black'))



#imputing gae with mean 
ageNotNA <- is.na(df.titanic$Age)
meanage <- mean(df.titanic$Age[!ageNotNA])

imputeAge <- function(age){
  if(is.na(age)){
    return (meanage)
  } else {
    return (age)
  }
}

cleanAge <- sapply(df.titanic$Age, imputeAge)

df.titanic$Age <- cleanAge

install.packages('ggplot2')
library(ggplot2)

ggplot(df.titanic, aes(Survived)) + geom_bar()

ggplot(df.titanic, aes(Age, fill = factor(Survived))) + geom_histogram(binwidth = 20)

ggplot(df.titanic, aes(Fare, fill = factor(Survived))) + geom_histogram(binwidth = 50)

ggplot(df.titanic, aes(SibSp, fill = factor(Survived))) + geom_bar(position="fill")

install.packages('dplyr')
library(dplyr)

df.titanic.clean <- select(df.titanic, -PassengerId, -Name, -Ticket, -Cabin )
str(df.titanic.clean)

#Turn survived and pcalss into factors
df.titanic.clean$Survived <- factor(df.titanic.clean$Survived)
df.titanic.clean$Pclass<- factor(df.titanic.clean$Pclass)


#Dividing into test set and training set 
df.titanic.train<-sample_frac(df.titanic.clean, 0.7)

#row numbers in the train set 
sid<-as.numeric(rownames(df.titanic.train)) # because rownames() returns character
df.titanic.test<-df.titanic.clean[-sid,]

#The logistic regression model 
log.model <- glm(Survived ~ ., family = binomial(link = logit),df.titanic.train)

summary(log.model)
#Facors were turned into dummy variables 

predicted.probabilities <- predict(log.model,df.titanic.test, type = 'response')
predicted.values <- ifelse(predicted.probabilities>0.5,1,0)

#Mean can work on true false as well 
mean(c(TRUE, FALSE)) #TRUE - 1, FALSE - 0 

misClassError <- mean(predicted.values != df.titanic.test$Survived)

#Confusion matrix
cm <- table(df.titanic.test$Survived,predicted.probabilities>0.5)


