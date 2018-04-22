
titanic <- read.csv("train.csv")
+str(titanic)
titanic <- read.csv("test.csv")
str(titanic)
titanic <- read.csv("train.csv")
str(titanic)
+#Q2 - Are there attributes with missing values? If there are fill the missing values with the mean value of that attribute
  any(is.na(titanic))
titanic[is.na(titanic)] <- colMeans(is.na(titanic), arr.ind=TRUE)
titanic[is.na(titanic)] <- colMeans(is.na(titanic), na.rm=TRUE)
titanic[is.na(titanic)] <- colMeans(which(is.na(titanic), na.rm=TRUE))
titanic[is.na(titanic)] <- colMeans(which(is.na(titanic), arr.ind=TRUE))
for(i in 1:ncol(titanic)){
  +titanic[is.na(titanic[,i]), i] <- mean(data[,i], na.rm = TRUE)
  +}
titanic
for(i in 1:ncol(titanic)){
  data[is.na(titanic[,i]), i] <- mean(titanic[,i], na.rm = TRUE)
  }
titanic[is.na(titanic)] <- colMeans(which(is.na(titanic), arr.ind=TRUE))
+titanic[is.na(titanic)] <- colMeans(titanic, which(is.na(titanic), arr.ind=TRUE))
+titanic[is.na(titanic)] <- colMeans(which(is.na(titanic), arr.ind=TRUE))
+titanic[is.na(titanic)] <- colMeans(which(is.na(titanic), arr.ind=TRUE), na.rm = TRUE)
+titanic[is.na(titanic)] <- colMeans(is.na(titanic),na.rm = TRUE)
+titanic[which(is.na(titanic))] <- colMeans(x=is.na(titanic), na.rm = TRUE)
+titanic[which(is.na(titanic))] <- colMeans(x=titanic, na.rm = TRUE)
+titanic$Age[which(is.na(titanic$Age))] <- colMeans(x=titanic$Age, na.rm = TRUE)
+titanic$Age[which(is.na(titanic$Age))] <- colMeans(x=is.na(titanic$Age), na.rm = TRUE)
+titanic$Age[which(is.na(titanic$Age))] <- mean(titanic$Age)
+any(is.na(titanic))
+#Q2 - Are there attributes with missing values? If there are fill the missing values with the mean value of that attribute
  +any(is.na(titanic))
+str(titanic)
+titanic
+mean(titanic$Age)
+mean(!any(is.na(titanic$Age)))
+mean(!is.na(titanic$Age))
+ageNotNa <- subset(titanic$Age, !any(is.na(titanic)))
+ageNotNa
+ageNotNa <- subset(titanic$Age, !is.na(titanic))
+ageNotNa
+ageNotNa <- subset(titanic$Age, !is.na(titanic$Age))
+ageNotNa
+any(is.na(ageNotNa))
+titanic$Age[which(is.na(titanic$Age))] <- mean(ageNotNa)
+any(is.na(titanic))
+titanic
+titanic$Age
+str(titanic)
+#Q3 - Turn Survival into a factor with the right values
  +titanic$Survived <- as.factor(titanic$Survived)
  +titanic$Survived <- factor(titanic$Survived, levels = c(0,1), labels = c("No", "Yes"))
  +any(is.na(titanic))
  +str(titanic)
  +#Q4 - Use ggplot histogram to investigate how Age, Fare and SibSp affect Survival
    +library(ggplot2)
  +titanic.histogram <- ggplot(titanic, aes(x = titanic$Age, y = titanic$Survived)) + geom_histogram()
  +histogramAS <- ggplot(titanic, aes(x = titanic$Age, y = titanic$Survived)) + geom_histogram()
  +histogramAS
  +histogramAS <- ggplot(titanic, aes(x = titanic$Age, y = titanic$Survived)) + geom_histogram(binwidth = 0.5, color = 'blue', fill = 'red', alpha = 0.2)
  +histogramAS
  +histogramAS <- ggplot(titanic, aes(x= titanic$Survived)) + geom_histogram(binwidth = titanic$Age, color = 'blue', fill = 'red', alpha = 0.2)
  +histogramAS
  +histogramAS <- ggplot(titanic, aes(x= titanic$Age)) + geom_histogram(binwidth = titanic$Survived , color = 'blue', fill = 'red', alpha = 0.2)
  +histogramAS
  +histogramAS <- ggplot(titanic, aes(x= titanic$Survived))
  +histogramAS
  +histogramAS <- ggplot(titanic, aes(x= titanic$Survived))
  +histogramAS <- ggplot(titanic, aes(x= titanic$Survived))
  ++ geom_histogram(data = titanic$Survived , color = 'blue', fill = 'red', alpha = 0.2)
  +histogramAS <- ggplot(titanic, aes(x= titanic$Survived))
  ++ geom_histogram(data = titanic$Survived , binwidth = 0.5 , color = 'blue', fill = 'red', alpha = 0.2)
  +histogramAS <- ggplot(titanic, aes(x= titanic$Survived)) + geom_histogram(data = titanic$Age , binwidth = 0.5 , color = 'blue', fill = 'red', alpha = 0.2)
  +histogramAS <- ggplot(titanic, aes(x= titanic$Survived)) + geom_histogram(aes(y= titanic$Age), color = 'blue', fill = 'red')
  +histogramAS
  +histogramAS <- ggplot(titanic, aes(x= titanic$Survived)) + geom_histogram(aes(color = 'blue')
                                                                             +histogramAS <- ggplot(titanic, aes(x= titanic$Survived)) + geom_histogram( color = 'blue')
                                                                             +histogramAS <- ggplot(titanic, aes(x= titanic$Survived)) + geom_histogram(color = 'blue')
                                                                             +histogramAS
                                                                             +ggplot(titanic,aes(x=titanic$Age)) +
                                                                               +geom_histogram(data=subset(titanic, titanic$Survived == 'No'),fill = "red", alpha = 0.2) +
                                                                               +geom_histogram(data=subset(titanic, titanic$Survived == 'Yes'),fill = "blue", alpha = 0.2)
                                                                             +ggplot(titanic,aes(x=titanic$Age)) +
                                                                               +geom_histogram(data=subset(titanic, titanic$Survived == 'No'),fill = "red", alpha = 0.2)
                                                                             +ggplot(titanic,aes(x=titanic$Age)) +
                                                                               +geom_histogram(data=subset(titanic, titanic$Survived == 'No'),fill = "red", alpha = 0.2, binwidth = 0.5)
                                                                             +ggplot(titanic,aes(x=titanic$Survived)) +
                                                                               +geom_histogram(data=subset(titanic, titanic$Age == 'No'),fill = "red", alpha = 0.2)
                                                                             +ggplot(titanic, aes(x=titanic$Survived, fill=titanic$Age)) + geom_histogram(alpha=0.2, position="identity")
                                                                             +ggplot(titanic, aes(x=titanic$Age, fill=titanic$Survived)) + geom_histogram(alpha=0.2, position="identity")
                                                                             +ggplot(titanic, aes(x=titanic$Age, fill=titanic$Survived)) + geom_histogram(alpha=0.2, position="identity", binwidth = 0.5)
                                                                             +histogramAS <- ggplot(titanic, aes(x=titanic$Age, fill=titanic$Survived)) + geom_histogram(alpha=0.2, position="identity", binwidth = 0.5)
                                                                             +#Fare & Survivel
                                                                               +histogramFS <- ggplot(titanic, aes(x=titanic$Fare, fill=titanic$Survived)) + geom_histogram(alpha=0.2, position="identity", binwidth = 0.5)
                                                                             +histogramFS
                                                                             +#Fare & Survivel
                                                                               +histogramFS <- ggplot(titanic, aes(x=titanic$Fare, fill=titanic$Survived)) + geom_histogram(alpha=0.2, position="identity", binwidth = 0.5) + xlim(0, 300)
                                                                             +histogramFS
                                                                             +#Fare & Survivel
                                                                               +histogramFS <- ggplot(titanic, aes(x=titanic$Fare, fill=titanic$Survived)) + geom_histogram(alpha=0.2, position="identity", binwidth = 0.5) + xlim(0, 500)
                                                                             +histogramFS
                                                                             +#Fare & Survivel
                                                                               +histogramFS <- ggplot(titanic, aes(x=titanic$Fare, fill=titanic$Survived)) + geom_histogram(alpha=0.2, position="identity", binwidth = 0.5) + xlim(0, 300)
                                                                             +histogramFS
                                                                             +#Silibings & Survivel
                                                                               +histogramFS <- ggplot(titanic, aes(x=titanic$SibSp, fill=titanic$Survived)) + geom_histogram(alpha=0.2, position="identity", binwidth = 0.5)
                                                                             +#Silibings & Survivel
                                                                               +histogramSS <- ggplot(titanic, aes(x=titanic$SibSp, fill=titanic$Survived)) + geom_histogram(alpha=0.2, position="identity", binwidth = 0.5)
                                                                             +histogramSS
                                                                             +#Q5 - Select relevant features for machine learning
                                                                               +sample <- sample.split(titanic$Survived, SplitRatio = 0.7)
                                                                             +#Q5 - Select relevant features for machine learning
                                                                               +library(caTools)
                                                                             +sample <- sample.split(titanic$Survived, SplitRatio = 0.7)
                                                                             +trainTitanic <- subset(titanic, sample)
                                                                             +testTitanic <- subset(titanic, !sample)
                                                                             +cor.data.train <- cor(titanic)
                                                                             +model <- lm(Survived ~ ., data = trainTitanic)
                                                                             +print(summary(model))
                                                                             +print(summary(trainTitanic))