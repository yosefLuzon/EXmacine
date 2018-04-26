adult<-read.csv("adult_sal.csv")
str(adult)
library(dplyr)
library(Amelia)
missmap(adult, main="Date", col=c('yellow','red'))
Europe <- c("England","Greece", "Hungary", "Ireland", "Holand-Netherlands",
            "Poland" , "Italy","Scotland", "Yugoslavia", "Germany" , "France",
            "Portugal")


Asia <- c("China" ,"Japan" , "Hong", "Cambodia", "Philippines",  "Iran", 
          "Taiwan", "Thailand" ,  "Laos", "India",  "Vietnam"    )


North.america <- c("United-States" ,  "Canada", "Puerto-Rico"  ) 

Latin.and.south.america <- c("Columbia", "Cuba", "Dominican-Republic",
                             "Ecuador", "El-Salvador",  "Guatemala", "Haiti" ,
                             "Honduras", "Jamaica", "Nicaragua", "Mexico","Peru",
                             "Outlying-US(Guam-USVI-etc)" ,"Trinadad&Tobago" )
Other <- c("South","?")
levels(adult$country)

adult.country<-as.character(adult$country)
imputcountryER<-function(country){
  if(country %in%  Europe){
    return("Europe")
  } else{
    return(country)
  }
}


imputcountryAs<-function(country){
  if(country %in%  Asia){
    return("Asia")
  } else{
    return(country)
  }
}

imputcountryNo<-function(country){
  if(country %in%  North.america){
    return("North.america")
  } else{
    return(country)
  }
}
imputcountryLa<-function(country){
  if(country %in%  Latin.and.south.america){
    return("Latin.and.south.america")
  } else{
    return(country)
  }
}
imputcountryOt<-function(country){
  if(country %in% Other ){
    return("Other")
  } else{
    return(country)
  }
}

adult.country<-lapply(adult.country,imputcountryER)
adult.country<-sapply(adult.country,imputcountryAs)
adult.country<-sapply(adult.country,imputcountryNo)
adult.country<-sapply(adult.country,imputcountryLa)
adult.country<-sapply(adult.country,imputcountryOt)
adult.country<-factor(adult.country)
adult$country<-adult.country

#qs3
levels(adult$marital)
Married<-c("Married-AF-spouse","Married-civ-spouse","Married-spouse-absent","Separated")
Former_Married<-c("Widowed","Divorced")

imputMarried<-function(marital){
  if(marital %in% Married ){
    return("Married")
  } else{
    return(marital)
  }
}
imputFormer<-function(marital){
  if(marital %in% Former_Married ){
    return("Former_Married")
  } else{
    return(marital)
  }
}

adult.marital<-as.character(adult$marital)
adult.marital<-sapply(adult.marital,imputMarried)
adult.marital<-sapply(adult.marital,imputFormer)
adult.marital<-factor(adult.marital)
adult$marital<-adult.marital

levels(adult$type_employer)
gov<-c("Federal-gov","Local-gov","State-gov")
not_work<-c("Never-worked","Without-pay")
self<-c("Self-emp-inc","Self-emp-not-inc")
Other <-c("?")

imputworkerGov<-function(type_employer){
  if(type_employer %in% gov ){
    return("Gov")
  } else{
    return(type_employer)
  }
}
imputworkernot_work<-function(type_employer){
  if(type_employer %in% not_work ){
    return("not_work")
  } else{
    return(type_employer)
  }
}
imputworkerSelf<-function(type_employer){
  if(type_employer %in% self ){
    return("Self")
  } else{
    return(type_employer)
  }
}
imputworker<-function(type_employer){
  if(type_employer %in% Other ){
    return("Other")
  } else{
    return(type_employer)
  }
}

adult.type_employer<-as.character(adult$type_employer)
adult.type_employer<-sapply(adult.type_employer,imputworkerGov)
adult.type_employer<-sapply(adult.type_employer,imputworkernot_work)
adult.type_employer<-sapply(adult.type_employer,imputworkerSelf)
adult.type_employer<-sapply(adult.type_employer,imputworker)

adult.type_employer<-factor(adult.type_employer)
adult$type_employer<-adult.type_employer
#qs5
adult_clean<-select(adult,-X)
str(adult_clean)
#qs6
zero<-c("<=50K")
one<-c(">50K")
imputincomezero<-function(income){
  if(income %in% zero ){
    return("0")
  } else{
    return(income)
  }
}
imputincomeone<-function(income){
  if(income %in% one ){
    return("1")
  } else{
    return(income)
  }
}
adult.income<-as.character(adult_clean$income)
adult.income<-sapply(adult.income,imputincomezero)
adult.income<-sapply(adult.income,imputincomeone)
adult.income<-factor(adult.income)
adult_clean$income<-adult.income
str(adult_clean)
ggplot(adult_clean,aes(country,fill=factor(income)))+geom_histogram(binwidth = 10)
ggplot(adult_clean,aes(country,fill=factor(income)))+geom_bar()
