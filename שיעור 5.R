bike<-read.csv("train.csv")
str(bike)
bike$datetime<-as.character(bike$datetime)
bike$date <-sapply(strsplit(bike$datetime,' '), "[", 1)
bike$date<-as.Date(bike$date)
bike$time <-sapply(strsplit(bike$datetime,' '), "[", 2)
bike$hour<-sapply(strsplit(bike$time,':'), "[", 1)
bike$hour<-as.numeric(bike$hour)
str(bike)

bike$time<-NULL
bike$season <- factor(bike$season, levels = c(1,2,3,4),labels=c('spring','summer','fall','winter'))
library (ggplot2)
library (ggplot2movies)

#A chart showing how temperature affects the count
pll2<-ggplot(bike,aes(x=temp,y=count))
pll2<-pll2+ geom_point(alpha=0.3, aes(color=temp))
pll2

#how date affact count
pll6<-ggplot(bike,aes(date,count))+geom_point(alpha=0.5, aes(color=temp))
pll6+scale_color_continuous(low="red",high="blue")

pll7<-ggplot(bike, aes(season,count)+geom_boxplot())
pll7
pll3<-boxplot(count~season,data=bike, main="season affects count", 
              xlab="Season", ylab="count")
pll3

#how hour affect on weekdays
pll8<-ggplot(bike [bike$workingday!=1,],aes(hour, count))
pll8<-pll8+geom_point(position = position_jitter(w=0.5, h=0), aes(color=temp))
pll8
#how hour affect on weekdays
pll9<-ggplot(bike [bike$workingday==1,],aes(hour, count))
pll9<-pll9+geom_point(position = position_jitter(w=0.5, h=0), aes(color=temp))
pll9

bike$datetime<-NULL
splitDate<-as.Date('2012-05-01')
dateFilter<-bike$date<=splitDate
bike.train<-bike[dateFilter,]
bike.test<-bike[!dateFilter,]

str(bike)

model<-lm(count ~. -atemp -casual -registered -date, bike.train)
summary(model)

predicted.train<-predict(model,bike.train)
predicted.train<-predict(model,bike.test)

MSE.train<-mean((bike.train$count-predicted.train)**2)
MSE.test<-mean((bike.test$count-predicted.train)**2)
RMSE.train<-MSE.train**0.5
RMSE.test<-MSE.test**0.5

