#qs1
air<-airquality

nrow(air)
#Qs2
row.has.na <- apply(air, 1, function(x){any(is.na(x))})
air2<- air[!row.has.na,]


#Qs3
v1<-c(mean(air2$Ozone),mean(air2$Solar.R),mean(air2$Wind),mean(air2$Temp))
v1

#Qs44
Temp$air2
air2.filter <- Temp$air2>77
