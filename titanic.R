# Load libraries
library(mlbench)
library(caret)
library(caretEnsemble)
library(DMwR)
library(Metrics)
library(plyr)

# Load the dataset
setwd("C:/Users/Zoey/Desktop")
train<-read.csv('train.csv',header=T, na.strings = c("NA", ""))
test<-read.csv('test.csv',header=T, na.strings = c("NA", ""))

#选取特征
test$Survived<-NA
all<-rbind(train,test)
features <- c("Pclass","Sex","Age","SibSp","Parch","Fare","Embarked",'Survived')
fea<- all[,features]

#处理缺失值
sapply(fea, function(x) {sum(is.na(x))})
fea$Age[is.na(fea$Age)]= median(fea$Age,na.rm = T)
fea$Fare[is.na(fea$ Fare)]=mean(fea$Fare,na.rm=T)
fea[is.na(fea)]<-0
fea$Sex = as.numeric(fea$Sex)
fea$Embarked = as.numeric(fea$Embarked)
fea$Survived<-factor(fea$Survived)

#划分训练集
ntrain <- fea[1:891,]
ntest <- fea[892:1309,1:7]
TrainData<-ntrain[,1:7]
TrainClasses <- ntrain[,8]
str(TrainData) 
TrainData[] <- lapply(TrainData, function(x) as.numeric(as.character(x)))

# 训练模型
 control <- trainControl(method="repeatedcv", number=5, repeats=3)
 metric <- "Accuracy"
 fittre <- caret::train(TrainData,TrainClasses, method="treebag", metric=metric, trControl=control)
 fitgbm <- caret::train(TrainData,TrainClasses,method='gbm', metric=metric,trControl=control)
 pre<- predict( fitgbm ,ntest,type = "raw")

 
 # Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
 solution <- data.frame(PassengerID = test$PassengerId, Survived = pre)
 write.csv(solution, file = 'Solution.csv', row.names = F)
 
 
 