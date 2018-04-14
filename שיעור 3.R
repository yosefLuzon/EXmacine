#קריאת המידע
worms<-read.table("worms.txt", header=T)
#סוג המידע בטבלה
str(worms)
#מיון רגיל
slope<-worms$Slope
worms[order(slope)]

#missing date
worms.missing<-read.table("worms.missing.txt", header=T)
any (is.na(worms.missing))
#clean
worms.clean<-na.omit(worms.missing)
any (is.na(worms.clean))

#Apply family of functions
#sapply, apply
v<-c(1,11,15,99,85)

cut60<-function(x){
  if(x<60){
  return(x)}
  else{
    return(60)
  }
}

vmax60<-sapply(v,cut60)

#applay
mat<- matrix(rnorm(24),8,3)
# 2 col' 1 row, 
mat.col.mean<-apply(mat,2,mean)
mat.row.mean<-apply(mat,1,mean)

cut1<-function(x){
  if(x<1){
    return(x)}
  else{
    return(1)
  }
}
#return vector
 tr1<-sapply(mat,cut1)
 
 students<-read.csv("student-mat.csv",sep=";")
 str(students)
any(is.na(students))
nrow(students)
#הפרדה של הקובץ
library(caTools)
install.packages('caTools')
smaple<-sample.split()
?sample.split

rows<-nrow(students)
set.seed=101
spilt1<-runif(rows)
split1<- split1 >= 0.3
students.train<-students[split,]
nrow(students.train)
