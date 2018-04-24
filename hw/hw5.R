adult<-read.csv("adult_sal.csv")
str(adult$country)
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
