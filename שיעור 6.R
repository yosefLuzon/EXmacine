titanic<-read.csv("titanic.csv")
str(titanic)
install.packages('dplyr')
library(dplyr)
install.packages('bindr')
install.packages('Amelia')
library(Amelia)
#מוציא מפה שמראה את מצב הDATA
missmap(titanic, main="Miising Date", col=c('yellow','red'))
#נישים ממוצע בערכים החסרים
AgeMissing<-is.na(titanic$Age)
meanage<-mean(titanic$Age[!AgeMissing])
#פונקצייה שמכניסה ת הממוצע של הגיל לאיפה שחסר נתונים
imputage<-function(age){
  if(is.na(age)){
    return(meanage)
  } else{
    return(age)
  }
}
#השמה בפועל
titanic$Age<-sapply(titanic$Age,imputage)

ggplot(titanic, aes(Survived))+geom_bar()

ggplot(titanic, aes(Age,fill=factor(Survived)))+geom_histogram(binwidth = 10)
ggplot(titanic, aes(Fare,fill=factor(Survived)))+geom_histogram(binwidth =20)



titanic_clean<-select(titanic,-PassengerId,-Name,-Cabin,-Ticket)
str(titanic_clean)

titanic_clean$Survived<-factor(titanic_clean$Survived)
titanic_clean$Pclass<-factor(titanic_clean$Pclass)
str(titanic_clean)
titanic_clean.train<-sample_frac(titanic_clean, 0.7)
sid<-as.numeric(rownames(titanic_clean.train))
titanic_clean.test<-titanic_clean[-sid,]

log.model<-glm(Survived ~., family = binomial(link='logit'),titanic_clean.train)
summary(log.model)

predicted.pro<-predict(log.model, titanic_clean.test, type='response')
predicted.values<-ifelse(predicted.pro>0.5,1,0)

missClassError<-mean(predicted.values!=titanic_clean.test$Survived)

