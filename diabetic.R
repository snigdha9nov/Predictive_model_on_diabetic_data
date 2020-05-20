

data<-read.csv("diabetes_v2 (2).csv",na.strings = c("","NA"))

#remove id variable
id<-data$id
data<-data[,-1]

#descriptive analysis
str(data)#struture od dataset
dim(data)#dimension of dataset
summary(data)#summary of dataset
colSums(is.na(data))#columnwise missing values

## Remove columns with more than 50% NA
data<-data[, -which(colMeans(is.na(data)) > 0.5)]
# remove na from target 
data<-data[!is.na(data$glyhb), ]


# Imputation of missing values by KNN
#install.packages("VIM")
library(VIM)
data1<-kNN(data,variable = colnames(data))
data <- subset(data1, select = -c(17:32))

#DAta Visulisation and outlier detection
library(funModeling)
library(tidyverse)


correlation_table(data,"glyhb")#correlation table


# visulisation and outlier detection of variable "stab.glu"
p <-ggplot(data, aes(glyhb,stab.glu))
p +geom_point()
#delete two point as outlier
data<-data[!(data$stab.glu>300 & data$glyhb<8),]
data<-data[!(data$stab.glu<100 & data$glyhb>12),]

# visulisation and outlier detection of variable "age"
p <-ggplot(data, aes(glyhb,age))
p +geom_point()
data<-data[data$age<100,]#deletion of outlier


# visualisation of "chol"
p <-ggplot(data, aes(glyhb,chol))
p +geom_point()

#visulisation and oulier detection in column "ratio"
p <-ggplot(data, aes(glyhb,ratio))
p +geom_point()
#delete the outlier which is greater than 15 ratio and greater than 12 glyhb
data<-data[!(data$ratio>15 & data$glyhb>12),]



#create new target variable "diabetic
data$diabetic<-ifelse(data$glyhb>7,"diabetic","non-diabetic")
data$diabetic<-as.factor(data$diabetic)
data<-data%>%select(-glyhb)

freq(data$diabetic)#frequencyplot of variable "diabetic"


#visualisation of variable "frame"
table(data$frame)
df2 <- data %>% 
  group_by(frame, diabetic) %>% 
  tally() %>% 
  complete(diabetic, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)
ggplot(df2, aes(diabetic, percentage, fill = frame)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

#visualisation of variable "gender"
table(data$gender)
df2 <- data %>% 
  group_by(gender, diabetic) %>% 
  tally() %>% 
  complete(diabetic, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)
ggplot(df2, aes(diabetic, percentage, fill = gender)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

#visualisation of variable "location
df2 <- data %>% 
  group_by(location, diabetic) %>% 
  tally() %>% 
  complete(diabetic, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)

ggplot(df2, aes(diabetic, percentage, fill = location)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()


#train-test split
set.seed(200)
index<-sample(nrow(data),0.70*nrow(data),replace = F)
train<-data[index,]
test<-data[-index,]



#model building
#Random forest
#install.packages("randomForest")
library(randomForest)
rf<-randomForest(diabetic~.,data = train)
rf

#validation with test data

#install.packages("irr")
library(irr)
#install.packages("caret")
library(caret)
#install.packages("e1071")
library(e1071)

#deploy the model with test dataset
prediction_rf<-predict(rf,test)

#confusion matrix to validate it
confusionMatrix(prediction_rf,test$diabetic)


#validation by ROC plot area under curve
#install.packages("ROCR")
library(ROCR)
#install.packages("pROC")
library(pROC)
#ROCR Curve
predict_rf<-prediction(as.numeric(test$diabetic),as.numeric(prediction_rf))
perf<-performance(predict_rf,"tpr","fpr")
plot(perf,col="red")
#Area under curve
auc(test$diabetic,as.numeric(prediction_rf ))

