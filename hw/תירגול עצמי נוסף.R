mtcars 
install.packages('caTools')
library(caTools)

install.packages('dplyr')
library(dplyr)
mpgM<-median(mtcars$mpg)
inputMpg<-function(mpg){
  if(mpg >mpgM){
    return('0')
  }else{return('1')}
}
mtcars$mpg<-sapply(mtcars$mpg,inputMpg)
mtcars$mpg<-as.numeric(mtcars$mpg)
install.packages('dplyr')
library(dplyr)
str(mtcars)
df.mtcars.clean <- select(mtcars, -disp, - hp, -drat, -qsec,-wt )
df.mtcars.clean$wt<-cut(df.mtcars.clean$w,breaks = c(0,3.3,6),levels=c("A","B"))
df.mtcars.clean$mpg<-as.factor(df.mtcars.clean$mpg)
str(df.mtcars.clean)


#The logistic regression model 
log.model <- glm(mpg ~ ., family = binomial(link = logit),df.mtcars.clean)
summary(log.model)


predicted.probabilities <- predict(log.model,df.mtcars.clean, type = 'response')
predicted.values <- ifelse(predicted.probabilities>0.5,1,0)
#Mean can work on true false as well 
mean(c(TRUE, FALSE)) #TRUE - 1, FALSE - 0 
df.mtcars.clean$mpg<-as.numeric(df.mtcars.clean$mpg)
inputMpg1<-function(mpg){
  if(mpg==2){
    return(1)}
  else{return(0)}
}
df.mtcars.clean$mpg<-sapply(df.mtcars.clean$mpg,inputMpg1)

misClassError <- mean(predicted.values - df.mtcars.clean$mpg)
