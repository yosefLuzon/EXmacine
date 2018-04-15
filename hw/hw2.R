bike<-read.csv("train.csv")
str(bike)
bike$datetime<-as.character(bike$datetime)
bike$date <-sapply(strsplit(bike$datetime,' '), "[", 1)
bike$date<-as.Date(bike$date)
bike$time <-sapply(strsplit(bike$datetime,' '), "[", 2)
bike$hour<-sapply(strsplit(bike$time,':'), "[", 1)
bike$hour<-as.numeric(bike$hour)
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


#A chart showing how season affects count
pll3<-boxplot(count~season,data=bike, main="season affects count", 
        xlab="Season", ylab="count")
pll3


pll4<-ggplot(bike,aes(x=hour,y=count))
pll4<-pll4 + geom_point(aes(color=temp))
pll4
heat.colors?
  
