#Adultd example 
#https://www.kaggle.com/sergeboo/adults-competition/data

adult_url <- read.csv(url('https://raw.githubusercontent.com/Geoyi/Salary-prediction/master/adult_sal.csv')) 

str(adult_url)

summary(adult_url)

levels(adult_url$type_employer)

table(adult_url$type_employer)

unemp <- function(job){
  job <- as.character(job)
  if(job == 'Never-worked' | job == 'Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer,unemp)

table(adult$type_employer)

str(adult)

adult_url <- select(adult_url,-x)

group_emp <- function(job){
  if(job == 'Local-gov' | job == 'State-gov' | job == 'Federal-gov' ){
    return('Gov')
  }else if(job == 'Self-emp-inc' | job == 'Self-emp-not-inc'){
    return('Self-emp')
  } else {
    return(job)
  }
}
adult$type_employer <- sapply(adult$type_employer,group_emp)



table(adult_url$marital)

group_marital <- function(mar){
  mar <- as.character(mar)
  if(mar == 'Separated' | mar == 'Divorced' | mar == 'Widowed'){
    return('Not-married')
  }else if(mar == 'Never-married'){
    return(mar)
  } else {
    return('Married')
  }
}

adult_url$marital <- sapply(adult_url$marital ,group_marital )


table(adult_url$marital)

levels(adult_url$country)

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
Other <- c("South")

group_country <- function(ctry){
  if(ctry %in%  Europe) 
  {return("Europe")} 
  else if (ctry %in% Asia)
  {return("Asia")}
  else if (ctry %in% North.america)
  {return("North.america")}
  else if (ctry %in% Latin.and.south.america)
  {return("Latin.and.south.america")}
  else 
  {return('Other')} 
}

adult_url$country <- sapply(adult$country,group_country )

table(adult_url$country)

adult_url$type_employer <- factor(adult_url$type_employer)
adult_url$country <- factor(adult_url$country)
adult_url$marital  <- factor(adult_url$marital)

str(adult_url)

adult_url[adult_url=='?'] <- NA

table(adult_url$type_employer)

missmap(adult)

adult_url <- na.omit(adult_url)

library(dplyr)

ggplot(adult_url, aes(age)) + geom_histogram(aes(fill = income),color= 'black', binwidth = 1)


levels(adult_url$income)

adult_url <- rename(adult_url, region = country)

str(adult_url)

adult_url <- select(adult_url, -x)

install.packages('caTools')
library(caTools)
set.seed(101)
sample <- sample.split(adult_url$income, SplitRatio = 0.7)

adult.train <- subset(adult_url, sample ==T)
adult.test <- subset(adult_url, sample ==F)

model.income <- glm(income ~ ., family = binomial(link = 'logit'), data = adult.train)

summary(model.income)
##

step.model.income <- step(model.income)

summary(step.model.income)

predicted.test.income <- predict(model.income, newdata = adult.test, type = 'response')

