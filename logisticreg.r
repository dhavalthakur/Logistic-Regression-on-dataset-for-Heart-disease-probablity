#Logistic Regression on UCI dataset for heart disease
library(ggplot2)
library(cowplot)
library(ggplot2)
dataset<-"http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
data <- read.csv(dataset, header=FALSE)
head(data)
#giving proper headers to the columns
colnames(data)<- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach",
                   "exang","oldpeak","slope","ca","thal","hd")
head(data)
str(data)

#Data Cleaning Steps
data[data=="?"] <- NA
data[data$sex==0,]$sex <-"F"
data[data$sex==1,]$sex <- "M"
data$sex <-as.factor(data$sex)  #converting the column as factor
data$slope <-as.factor(data$slope)
data$restecg <-as.factor(data$restecg)
data$exang <-as.factor(data$exang)
data$fbs <-as.factor(data$fbs)
#converting first this column to integer and then to factor
data$ca <-as.integer(data$ca)
data$ca <-as.factor(data$ca)
#same conversion here
data$thal <-as.integer(data$thal)
data$thal <-as.factor(data$thal)
data[data$hd==0,]$hd <-"Healthy"
data[data$hd==1,]$hd <-"Unhealthy"
data[data$hd==2,]$hd <-"Unhealthy"
data[data$hd==3,]$hd <-"Unhealthy"
data[data$hd==4,]$hd <-"Unhealthy"
data$hd<-as.factor(data$hd)
head(data)
#Checking for NA values
nrow(data[is.na(data$ca) | is.na(data$thal),])
#Checking the rows with NA's
data[is.na(data$ca) | is.na(data$thal),]
nrow(data) #Current total number of samples
data<- data[!(is.na(data$ca)| is.na(data$thal)),]
nrow(data)
xtabs(~hd+ sex,data=data)
xtabs(~hd +cp ,data=data)
xtabs(~hd+ restecg,data=data)
xtabs(~hd+ fbs,data=data)
xtabs(~hd+ exang,data=data)
xtabs(~hd+ slope,data=data)
xtabs(~hd+ thal,data=data)
xtabs(~hd+ ca,data=data)
#Logistic Regression : Model 1: Predicting disease using only the gender of each patient
logistic <-glm(hd ~sex,data=data,family="binomial")
summary(logistic)
logistic <-glm(hd ~ .,data=data,family="binomial")
summary(logistic)
#From here we see that Age cannot be a good predictor as it has large P Value
ll.null<- logistic$null.deviance/-2
ll.proposed <-logistic$deviance/-2
(ll.null - ll.proposed)/ll.null  #Overall effect size

## now we can plot the data
predicted.data <- data.frame(probability.of.hd=logistic$fitted.values,
  hd=data$hd)
predicted.data <- predicted.data[
  order(predicted.data$probability.of.hd, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)
ggplot(data=predicted.data, aes(x=rank, y=probability.of.hd)) +
  geom_point(aes(color=hd), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of getting a heart disease")
ggsave("heart_disease_probabilities.pdf")