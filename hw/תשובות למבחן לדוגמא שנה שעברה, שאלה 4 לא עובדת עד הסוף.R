
mtcars
mpgM<-median(mtcars$mpg)
inputMpg<-function(mpg){
  if(mtcars$mpg >"19.2"){
    return('yes')
  }else{return('no')}
}

mtcars$mpg<-sapply(mtcars$mpg,inputMpg)
mtcars$mpg<-as.factor(mtcars$mpg)
##2
install.packages('rpart')
library(rpart)

install.packages('rpart.plot')
library(rpart.plot)

install.packages('ISLR')
library('ISLR')
str(mtcars)
tree<-rpart(mpg~ . , mtcars)
prp(tree)

##3
install.packages('cluster')
library(cluster)

ng<-kmeans(mtcars[ ,2:11],2, nstart = 20)
ng1<-kmeans(mtcars[ ,2:11],4, nstart = 20)
ng$cluster
table(ng$cluster,mtcars$mpg )
ng1$cluster
table(ng1$cluster,mtcars$mpg )

ng3<-kmeans(mtcars[ ,2:11],3, nstart = 20)
ng3$cluster
table(ng3$cluster,mtcars$mpg )
clusplot(mtcars,ng3$cluster, color=T, shade=T, labels=0, lines=0)

clusplot(mtcars,ng$cluster, color=T, shade=T, labels=0, lines=0)
clusplot(mtcars,ng1$cluster, color=T, shade=T, labels=0, lines=0)
##4
str(mtcars$wt)
head(mtcars$wt)
mtcars$wt<-cut(mtcars$wt,breaks = c(0,3.3,6),levels=c("A","B"))
