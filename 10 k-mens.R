iris
install.packages('ggplot2')
library (ggplot2)
pl<-ggplot(iris, aes(petal, Length, color=Species))
pl<-pl+geom_point(size=4)

pl<-ggplot(iris, aes(petal, Length, color=Species))
pl<-pl+geom_point(size=4)

set.seed(101)
irisClaster<-kmeans(iris[,1:4], 3, nstart=20)
irisClaster$cluster
table(irisClaster$cluster, iris$Species)

install.packages('cluster')
library (cluster)
clusplot(iris,irisClaster$cluster, color = 1, )