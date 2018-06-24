mtcars
mpgM<-median(mtcars$mpg)
inputMpg<-function(mpg){
  if(mpg >mpgM){
    return('0')
  }else{return('1')}
}
mtcars$mpg<-sapply(mtcars$mpg,inputMpg)
any(is.na(mtcars))
cols.numeric <- sapply(mtcars,is.numeric)
mtcars$mpg<-as.numeric(mtcars$mpg)
cor.data <- cor(mtcars)                      
plot(mtcars$mpg,mtcars$cyl)
plot(mtcars$disp,mtcars$cyl )
hist(mtcars[,'mpg'], col = 'blue')
model <- lm(mpg ~ ., data = mtcars)
summary(model)
