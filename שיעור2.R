v1<-c(1,2,3,4,5,6)
v2<-c(7,8,9,10,11,12)
mat1 <- rbind(v1,v2)
mat2<-cbind(v1,v2)
mat1
class<-(mat1)
#random numbers
# normal distu
dnorm(0,0,0)
qnorm(0.5,mean=0,sd=1)
#������� ������� ���� ���� �� ���� �2
pnorm(1,0,1)
#����� ����� ������� ��������
rnorm(10,0,1)
# ������-10, ������� ��������-����� 0 
mean(rnorm(1000000,0,1))#��� ������ �������
#����� ����� ������� ����� ��������� ������� 0 ������� 1
e<-runif(5)*10

#�����
data<-matrix(c(1:6,c(10,11,15,17,55,78)),6,2)
#����� ��� �� ����� 1 ��� ����� 2
plot(data[,1], data[,2])
data
#�� ����� �� ����� ��� L, �� ��=����� ������ ������ ����� ����� �� ����.
plot(data[,1], data[,2],type = "l", ylim=c(0,60))
#���������
#������ �������� �����
u<-runif(1000,0,100)
#����� ���������
hist(u, ylim=c(1,50))
n<-rnorm(1000,0,100)
hist(n)
#��� ����� ��� ��� ���� ���� ����� �� ��������� par
par(mfrow=c(2,1))
plot(seq(-4,4,0.01), dnorm(seq(-4,4,0.01)),type="l")
plot(seq(-4,4,0.1), dnorm(seq(-4,4,0.1),mean=0,sd=0.5),type="l")
#����� ��������
toFar<-function(cel){
  x<-cel*1.8
x+32}
#���� ������ �� ��������� ���� ���� �� ��� ����� �����
toFar(33)
toFar1<-function(cel){
  x<-cel*1.8+32
  return(x)}
toFar1(33)

#�������� �����
derivf<-function(x){
  (f(x+0.1)-f(x))/0.01
}
f<-function(x){
  x^3+X+10
}
derivf(5)



#lists
l<-list(owner='jack', sum=3000)
l[[1]]

#$notation
l$sum

#data frame ����� ������ ��� DB ���� �� ������ ������� ���� ������ ��� ���� ����� ����� ����� ����� ����� ���.
brands<-c('Ford','Mazda','Fiat')
from<-c('us','japan','italy')
rank<-c(3,2,1)
cars<-data.frame(brands,from,rank)
cars$brands
#levels? ��� �� ��� ������ ������ �� 3 ������� ��� ����� �����.
cars$rank
typeof(cars$from)
#filters
brands.filter<-cars$brands=='Fiat'
cars[brands.filter,1:2]
#���� �� ������
summary(cars)
#lavel �r  ��� ������ ������
str(cars)
#��� ������ ���� �����?
worms<-read.table("worms.txt", header = T)
worms$Damp
typeof(worms$Damp)
