
install.packages("DataExplorer")
install.packages("Hmisc")
install.packages("data.table")
install.packages("caret")

install.packages("extrafont")
install.packages("ggthemes")

install.packages("caret",
                 repos = "http://cran.r-project.org", 
                 dependencies = c("Depends", "Imports", "Suggests"))

library(caret)
library(data.table)
library(Hmisc)
library(DataExplorer)
library(ggplot2)
library(carData)
library(car)
library(dplyr)
library(lattice)
library(tidyr)
library(caret)
library(MASS)
library(broom)
library(ROCR)
library(corrplot)


setwd("C:/Users/Arvind/Desktop/Projects/Heart disease/")
heart_data= read.csv("heart.csv")
head(heart_data)
names(heart_data)
colnames(heart_data)[colnames(heart_data)=="ï..age"]<- "age"
factor_heart_data<- copy(heart_data)
colnames(factor_heart_data)[colnames(factor_heart_data)=="ï..age"]<- "age"
head(factor_heart_data)

col_names <- c("age","sex","chest_pain","rest_bp","chol","fasting_bloodsugar","rest_ecg","max_heartrate",
          "exercise_angina","ST_depression","slope","n_major_vasel","thal","target")

names(factor_heart_data)<- col_names
names(factor_heart_data)
head(factor_heart_data)
#View(factor_heart_data)

cor_heart = cor(factor_heart_data)
corrplot(cor_heart, method = "ellipse", type="upper")

factor_heart_data$sex <- as.character(heart_data$sex)
factor_heart_data$sex <- ifelse(heart_data$sex=="0", 'female', 'male')
factor_heart_data$chest_pain<-as.factor(heart_data$cp)

unique(heart_data$cp)
factor_heart_data$fasting_bloodsugar[heart_data$fasting_bloodsugar == 1]= "Diabetic"
factor_heart_data$fasting_bloodsugar[heart_data$fasting_bloodsugar == 0] = "Normal"

factor_heart_data$rest_ecg[heart_data$rest_ecg == 0] = "Normal"
factor_heart_data$rest_ecg[heart_data$rest_ecg == 1] = "Abnormality"
factor_heart_data$rest_ecg[heart_data$rest_ecg == 2] = "Probable or definite"

factor_heart_data$exercise_angina[heart_data$exercise_angina == "1"]= "yes"
factor_heart_data$exercise_angina[heart_data$exercise_angina == "0"] = "no"

factor_heart_data$slope=as.factor(heart_data$slope)
factor_heart_data$thal=as.factor(heart_data$thal)
factor_heart_data$target=as.factor(heart_data$target)
factor_heart_data$sex=as.factor(heart_data$sex)
factor_heart_data$fasting_bloodsugar=as.factor(heart_data$fbs)
factor_heart_data$exercise_angina=as.factor(heart_data$exang)

#View(factor_heart_data)
head(factor_heart_data)
str(factor_heart_data)
summary(factor_heart_data)

duplicated(factor_heart_data)


describe(factor_heart_data)

plot_histogram(factor_heart_data)

plot_density(factor_heart_data)

plot_correlation(heart_data)

plot_correlation(factor_heart_data)



#heart rate
#Heart Disease is uniformly spread out across Age
colnames(heart_data)
str(heart_data)



ggplot(factor_heart_data,aes(age, fill=target)) +
  geom_histogram(aes(y=..density..),breaks=seq(0, 80, by=1), color="grey17") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~target, ncol=1,scale="fixed") +
  scale_fill_manual(values=c("blue","red"))+
  xlab("Age") +
  ylab("Density / Count") +
  ggtitle("Age Distribution")
str(heart_data)


#comparing men and women with heart disease
colnames(heart_data)[colnames(heart_data)=="ï..age"]<-"Age"
head(heart_data)
ggplot(factor_heart_data,aes(target, fill=target)) + 
  geom_bar(stat = "count") + facet_wrap(sex~.) + scale_fill_manual(values=c("Blue","red")) +
  ggtitle("Gender count for Heart Disease")

#More Heart Disease patients have chest pain type 1 or 2
str(factor_heart_data)
ggplot(factor_heart_data,aes(target,fill=target)) +
  geom_bar(stat = "count") + facet_wrap(chest_pain~.) + ggtitle("Count of Heart Patients having different types of chest Pains") + theme_bw() +
  scale_fill_manual(values=c("Blue","red"))+
  xlab("Target")

#Patients with Rest ECG 1 have more Heart Diseases: 0-healthy 1-unhealthy
str(factor_heart_data)
ggplot(factor_heart_data,aes(target,fill=target)) +
  geom_bar(stat = "count") + facet_wrap(rest_ecg~.)+
  ggtitle("Patients with different types of ECG test")+
  scale_fill_manual(values=c("Blue","red"))


#cholestrol
ggplot(factor_heart_data,aes(factor_heart_data$chol, fill=target)) +
  geom_histogram(aes(y=..density..),breaks=seq(90, 550, by=25), color="grey17") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~target, ncol=1,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("blue","red")) +
  xlab("Cholestoral levels") +
  ylab("Density / Count") +
  ggtitle("Cholestrol level test")

#maximum heart rate
ggplot(factor_heart_data,aes(factor_heart_data$max_heartrate, fill=target)) +
  geom_histogram(aes(y=..density..),breaks=seq(70, 205, by=10), color="grey17") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~target, ncol=1,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("blue","red"))+
  xlab("Maximum Heart Rate Achieved") +
  ylab("Density / Count") +
  ggtitle("Max Heart Rate Histogram")




#logirsic regression

set.seed(12345)
train <- floor(0.75*nrow(factor_heart_data))

train_ind <-sample(seq_len(nrow(factor_heart_data)),size = train)
trainset <- factor_heart_data[train_ind, ]

testset <- factor_heart_data[-train_ind, ]
dim(trainset)
dim(testset)


logit<-glm(target~., data=trainset, family = binomial)
summary(logit)


cor_data<-trainset[,c(2,3,9,10,12,14)]
summary(cor_data)

#variance inflation factor
vif(glm(target ~ ., data=cor_data, family="binomial"))

logit1<-glm(target~., data=cor_data, family=binomial)
summary(logit1)


cor_data$chest_pain<-as.factor(cor_data$chest_pain)

factor_heart_data$chest_pain<-as.factor(heart_data$cp)

logit1.df<-tidy(logit1)

library(ggthemes)
library(extrafont)
logit1.df %>%
  mutate(term=reorder(term,estimate)) %>%
  ggplot(aes(term,estimate, fill=estimate))+
  geom_bar(stat="identity")+
  ggtitle("Effect of variables resulting Heart Disease")+
  scale_fill_gradient(low="blue", high="red")+
  theme_economist()+
  geom_hline(yintercept=0)+
  coord_flip()


#Tuning parameters
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)


trainset$target<-make.names(trainset$target)
set.seed(142)
trainset$target<-as.factor(trainset$target)

generalized_model <- caret::train(target ~ ., 
                          data = trainset ,
                          method = "glm", 
                          trControl = fitControl,
                          metric="ROC")

generalized_model


#Variable importance.

varImp(generalized_model)

pred <- predict(generalized_model,testset,type='raw')
summary(pred)
pred
testset$target



Confusion_Matrix<-table(testset$target, pred)
Confusion_Matrix

accuracy<-sum(diag(Confusion_Matrix))/sum(Confusion_Matrix)
accuracy
#calculating Missclassification rate
1-sum(diag(Confusion_Matrix))/sum(Confusion_Matrix)









