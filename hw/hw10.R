wineRed <- read.csv("wine1.CSV", sep = ";")
wineWaite <- read.csv("wine2.CSV", sep = ";")
install.packages('ggplot2')
library (ggplot2)
install.packages('ggplot2movies')
library (ggplot2movies)
install.packages('ISLR')
install.packages('cluster')
library (cluster)
install.packages('ggplot2')
library (ggplot2)
###
str(wineRed)
str(wineWaite)
wineRed$type="red"
wineWaite$type="white"
winetotal<-rbind(wineRed,wineWaite)
str(winetotal)
levels(winetotal$type)
winetotal$type<-as.factor(winetotal$type)
##
pl<-ggplot(winetotal,aes(x=residual.sugar))
pl<-pl+geom_histogram(binwidth=3,color='blue', fill='red', alpha='0.2')
pl<-pl+ggtitle("A histogram")
pl

###
pl11<-ggplot(winetotal,aes(x=alcohol))
pl11<-pl11+geom_histogram(binwidth=3,color='blue', fill='red', alpha='0.2')
pl11<-pl11+ggtitle("B histogram")
pl11

####
winetotal11<-kmeans(winetotal[ ,1:12],2, nstart = 10)

install.packages('cluster')
library(cluster)

clusplot(winetotal,winetotal11$cluster, color=T, shade=T, labels=0, lines=0)




