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
#הסתברות שהתוצאה תהיה קטנה או שווה ל2
pnorm(1,0,1)
#דגימה מיתוך התפלגות נורמאלית
rnorm(10,0,1)
# דגימות-10, התפלגות נורמאלית-ממוצע 0 
mean(rnorm(1000000,0,1))#חוק מספרים הגדולים
#דגימה מיתוך התפלגות אחידה דיפולטיבי מינימום 0 מקסימום 1
e<-runif(5)*10

#גרפים
data<-matrix(c(1:6,c(10,11,15,17,55,78)),6,2)
#יצירת גרף של עמודה 1 מול עמודה 2
plot(data[,1], data[,2])
data
#עמ ליצור קו נוסיף סוג L, עמ לנ=התחיל מנקודה מסוימת נוסיף משתנה של גבול.
plot(data[,1], data[,2],type = "l", ylim=c(0,60))
#היסטורגמה
#דגימות מהתפלגות אחידה
u<-runif(1000,0,100)
#בניית ההיסטוגרם
hist(u, ylim=c(1,50))
n<-rnorm(1000,0,100)
hist(n)
#שני גרפים אחד ליד השני חייב להריץ את הפונקצייה par
par(mfrow=c(2,1))
plot(seq(-4,4,0.01), dnorm(seq(-4,4,0.01)),type="l")
plot(seq(-4,4,0.1), dnorm(seq(-4,4,0.1),mean=0,sd=0.5),type="l")
#יצירת פונקצייה
toFar<-function(cel){
  x<-cel*1.8
x+32}
#אחרי הכתיבה של הפונקצייה נריץ אותה על מנת שתהיה זמינה
toFar(33)
toFar1<-function(cel){
  x<-cel*1.8+32
  return(x)}
toFar1(33)

#פונקציית גזירה
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

#data frame מיבנה נתונים כמו DB כאשר כל ווקטור במטריצה יכול להחזיק סוג מידע אחרכל עמודה חייבת להיות מאותו סוג.
brands<-c('Ford','Mazda','Fiat')
from<-c('us','japan','italy')
rank<-c(3,2,1)
cars<-data.frame(brands,from,rank)
cars$brands
#levels? האם הם חלק מקבוצה מובנית של 3 אבריםאו שזה שלושה ערכים.
cars$rank
typeof(cars$from)
#filters
brands.filter<-cars$brands=='Fiat'
cars[brands.filter,1:2]
#מידע על הרשימה
summary(cars)
#lavel שr  נתן לערכים ברשימה
str(cars)
#איך מיבאים קובץ מבחוץ?
worms<-read.table("worms.txt", header = T)
worms$Damp
typeof(worms$Damp)
