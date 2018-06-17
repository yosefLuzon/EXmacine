iris
install.packages('ISLR')
install.packages('cluster')
library (cluster)
install.packages('ggplot2')
library (ggplot2)
iric1<-kmeans(iris[ ,1:4],3, nstart = 20)
clusplot(iris,iric1$cluster, color=T , shade=T, labels=0, lines=0)



