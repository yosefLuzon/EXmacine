bike<-read.csv("train.csv")
str(bike)
bike$datetime<-as.character(bike$datetime)
bike$date <-sapply(strsplit(bike$datetime,' '), "[", 1)
bike$date<-as.Date(bike$date)
bike$time <-sapply(strsplit(bike$datetime,' '), "[", 2)
bike$hour<-sapply(strsplit(bike$time,':'), "[", 1)
bike$hour<-as.numeric(bike$hour)
bike$season <- factor(bike$season, levels = c(1,2,3,4), labels = c('spring', 'summer', 'fall','winter' ))

str(bike)


library (ggplot2)
install.packages('ggplot2movies')
library (ggplot2movies)

#A chart showing how date affects count 
pll<-ggplot(bike,aes(x=date,y=count))
pll<-pll+ geom_point(alpha=0.2)
pll

#A chart showing how temperature affects the count
pll2<-ggplot(bike,aes(x=temp,y=count))
pll2<-pll2+ geom_point(alpha=0.2)
pll2

cor(bike[,c('temp', 'count')])

#What regression line look like
plot(bike$temp,bike$count)
abline(lm(bike$count ~ bike$temp))


#A chart showing how season affects count
pll3<-boxplot(count~season,data=bike, main="season affects count", 
        xlab="Season", ylab="count")
pll3


pll4<-ggplot(bike,aes(x=hour,y=count))
pll4<-pll4 + geom_point(aes(color=temp))
pll4
#Split into training set snd test set 
splitDate <- as.Date("2012-05-01")
dateFilter <- bike$date <= splitDate 

bike.train <- bike[dateFilter,]
bike.test <-  bike[!dateFilter,]

nrow(bike.train)
nrow(bike.test)
bike.train$datetime <-NULL
bike.test$datetime <-NULL
model <- lm(count ~ . -atemp -casual -registered -date, bike.train)
model2 <- lm(count ~ . -atemp -casual -registered -date, bike.test)
summary(model)
predicted.train <- predict(model,bike.train)
predicted.test <- predict(model2,bike.test)


MSE.train <- mean((bike.train$count - predicted.train)**2)
MSE.test <- mean((bike.test$count - predicted.test)**2)

RMSE.train <- MSE.train**0.5
RMSE.test <- MSE.test**0.5


  
  
