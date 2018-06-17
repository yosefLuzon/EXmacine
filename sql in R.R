 #sql
iris
install.packages('RSQLite')
library (RSQLite)
install.packages('sqldf')
library (sqldf)

#select
str(iris)
iris$Petal_Length<- iris$Petal.Length

setosa_Large<-sqldf('
SELECT * FROM iris i1
WHERE i1.species="setosa"
AND i1.Petal_Length >1.3
                    ')
