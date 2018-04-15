install.packages('ggplot2')
library (ggplot2)
install.packages('ggplot2movies')
library (ggplot2movies)

str(movies)
pl<-ggplot(movies,aes(x=rating))
pl<-pl+geom_histogram(binwidth=0.5,color='blue', fill='red', alpha='0.2')
pl<-pl+ggtitle("A histogram")


pll<-ggplot(movies,aes(x=length,y=rating))
pll<-pll+ geom_point(alpha=0.2)
pll<-pll+xlim(0,250)

pl2<-ggplot(movies,aes(x=year,y=rating))
pl2<-pl2+geom_bin2d()+scale_fill_gradient(low='green', high="red")

str(mtcars)
ca<-ggplot(mtcars,aes( x=wt, y=mpg))
ca<-ca + geom_point(aes(shape=factor(cyl)), size=5)
 ca
str(mpg)
mpg1<- ggplot(mpg, aes(x=class))                   
mpg1<- mpg1+ geom_bar(color='blue', fill='light blue')
mpg1<-mpg1+geom_bar(aes(fill=drv))
mpg1<-mpg1+geom_bar(aes(fill=drv),position = 'fill')

